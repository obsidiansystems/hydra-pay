{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Backend

(backend)

where

import Prelude hiding (filter)

import Hydra.Types
import Hydra.Devnet

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core

import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Foldable

import Data.String.Interpolate ( i )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc

import qualified System.IO.Streams as Streams

import Control.Concurrent
import System.Process

import Data.Aeson as Aeson
    ( decode, encode, FromJSON, ToJSON )

import qualified Data.ByteString.Lazy as LBS

import HydraPay
import HydraPay.WebSocket
import CardanoNodeInfo

import HydraPay.Api
import Control.Monad ((<=<), forever, when, guard)

import Network.WebSockets.Snap
import qualified Network.WebSockets as WS
import Data.Traversable (forM)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe
import System.Directory (doesFileExist)
import qualified HydraPay.WebSocketDemo as WSD

getDevnetHydraSharedInfo :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraSharedInfo
getDevnetHydraSharedInfo = do
  scripts <- getReferenceScripts "devnet/scripts" (_signingKey devnetFaucetKeys)
  pure $ HydraSharedInfo
    { _hydraScriptsTxId = T.unpack scripts,
      _cardanoNodeInfo = cardanoDevnetNodeInfo
    }

cardanoDevnetNodeInfo :: CardanoNodeInfo
cardanoDevnetNodeInfo = CardanoNodeInfo (TestNet 42) "devnet/node.socket" "devnet/protocol-parameters.json" "devnet/genesis-shelley.json"

devnetFaucetKeys :: KeyPair
devnetFaucetKeys = mkKeyPair "devnet/credentials/faucet.sk" "devnet/credentials/faucet.vk"

-- TODO: See if it's okay to change Either a (Maybe b) to Just Either a b.
-- What does writing toJSON () to response do?
handleJsonRequestBody :: (MonadSnap m, ToJSON a,
                          FromJSON t, ToJSON b) =>
  (t -> LoggingT (WithSeverity (Doc ann)) IO (Either a (Maybe b))) ->
  m ()
handleJsonRequestBody f = do
  runRequestBody Streams.toList
    >>= ( \case
            Nothing -> do
              modifyResponse $ setResponseStatus 400 "Bad Request"
              writeLBS $ Aeson.encode InvalidPayload
            Just x -> do
              result <- liftIO $ withLogging $ f x
              case result of
                Right a -> mapM_ (writeLBS . Aeson.encode) a
                Left err -> writeLBS $ Aeson.encode err
        )
      . Aeson.decode
      . LBS.fromChunks

data DemoConfig
  = CfgDevnet
  | CfgPreview PreviewDemoConfig
  deriving (Show,Read)

data PreviewDemoConfig = PreviewDemoConfig
  { _previewNodeInfo :: CardanoNodeInfo
  , _previewHydraInfo :: HydraSharedInfo
  , _previewFaucet :: KeyPair
  , _previewParticipants :: [KeyPair]
  }
  deriving (Show,Read)

demoCfgPath :: FilePath
demoCfgPath = "democonfig"

writeDemoConfig :: DemoConfig -> IO ()
writeDemoConfig cfg = do
  writeFile demoCfgPath . show $ cfg

readDemoConfig :: IO DemoConfig
readDemoConfig = fmap (fromMaybe CfgDevnet) . runMaybeT $ do
  guard <=< liftIO $ doesFileExist demoCfgPath
  MaybeT $ readMaybe <$> readFile demoCfgPath

seedDemoAddressesPreview :: PreviewDemoConfig -> Lovelace -> IO ()
seedDemoAddressesPreview cfg amount = flip runLoggingT (print . renderWithSeverity id) $ do
  forM_ (_previewParticipants cfg) $ \kp -> do
    addr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey kp)
    seedAddressFromFaucetAndWait (_previewNodeInfo cfg) (_previewFaucet $ cfg) addr amount False

deseedDemoAddressesPreview :: PreviewDemoConfig -> IO ()
deseedDemoAddressesPreview cfg =
  forM_ (_previewParticipants cfg) $ \kp -> do
    addr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey kp)
    faucetAddr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey (_previewFaucet cfg))
    transferAll (_previewNodeInfo cfg) (_signingKey kp) addr faucetAddr


whenDevnet :: (Applicative m) => DemoConfig -> m () -> m ()
whenDevnet cfg = when (case cfg of
                          CfgDevnet -> True
                          _ -> False)

runLogging = flip runLoggingT (print . renderWithSeverity id)

withCardanoNode :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m, _) =>
  (State -> CardanoNodeInfo -> [(KeyPair, Address)] -> LoggingT (WithSeverity (Doc ann)) IO a) -> m a
withCardanoNode f = do
  cfg <- liftIO readDemoConfig
  case cfg of
    CfgDevnet -> do
      prepareDevnet
      liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ _stdout _ _handle -> do
        flip runLoggingT (print . renderWithSeverity id) $ do
          liftIO $ threadDelay (seconds 3)
          logMessage $ WithSeverity Informational [i|
            Cardano node is running
            |]
          prefix <- liftIO getTempPath'
          oneKs <- generateCardanoKeys $ prefix <> "one"
          one <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey oneKs
          twoKs <- generateCardanoKeys $ prefix <> "two"
          two <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey twoKs
          seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys one (ada 10000) False
          seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys two (ada 10000) False
          state <- getHydraPayState =<< getDevnetHydraSharedInfo
          f state cardanoDevnetNodeInfo [(oneKs, one), (twoKs, two)]
    CfgPreview pcfg -> liftIO . flip runLoggingT (print . renderWithSeverity id) $ do
      state <- getHydraPayState (_previewHydraInfo pcfg)
      participants <- forM (_previewParticipants pcfg) $ \kp -> do
        addr <- liftIO $ getCardanoAddress (_previewNodeInfo pcfg) (_verificationKey kp)
        pure (kp, addr)
      f state (_previewNodeInfo pcfg) participants
                    

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- NOTE(skylar): Running heads is a map from head name to network handle
      flip runLoggingT (print . renderWithSeverity id) $ do
        withCardanoNode $ \state cninf (participants@[(ks1, addr1), (ks2, addr2)]) -> do
            let addrs = snd <$> participants
            logMessage $ WithSeverity Informational [i|
              Serving
              |]
            liftIO . serve $ \case
              BackendRoute_HydraPay :/ hpr -> case hpr of
                HydraPayRoute_Init :/ () -> do
                  handleJsonRequestBody (fmap ((Nothing :: Maybe ()) <$) . initHead state)
                HydraPayRoute_Commit :/ () -> do
                  handleJsonRequestBody (fmap ((Nothing :: Maybe ()) <$) . commitToHead state)
                HydraPayRoute_AddFuelTx :/ (addr, amount) -> do
                  result <- buildAddTx Fuel state addr amount
                  case result of
                    Left err -> writeLBS $ Aeson.encode err
                    Right tx -> writeLBS $ Aeson.encode tx

                HydraPayRoute_AddFundsTx :/ (addr, amount) -> do
                  result <- buildAddTx Funds state addr amount
                  case result of
                    Left err -> writeLBS $ Aeson.encode err
                    Right tx -> writeLBS $ Aeson.encode tx

                HydraPayRoute_HeadStatus :/ name -> do
                  status <- getHeadStatus state name
                  writeLBS $ Aeson.encode status

                HydraPayRoute_Close :/ name -> do
                  status <- closeHead state name
                  writeLBS $ Aeson.encode status

                HydraPayRoute_Withdraw :/ () -> do
                  runRequestBody Streams.toList >>= (\case
                    Nothing -> do
                      modifyResponse $ setResponseStatus 400 "Bad Request"
                      writeLBS $ Aeson.encode InvalidPayload

                    Just wr -> do
                      result <- liftIO $ withLogging $ withdraw state wr
                      case result of
                        Right txid -> writeLBS $ Aeson.encode txid
                        Left err -> writeLBS $ Aeson.encode err) . Aeson.decode . LBS.fromChunks

                HydraPayRoute_SubmitTx :/ addr ->
                  handleJsonRequestBody ((fmap . fmap) Just . submitTxOnHead state addr)

                HydraPayRoute_Head :/ () -> do
                  handleJsonRequestBody $
                    fmap (Just <$>)
                    . mapM (getHeadStatus state . _head_name)
                    <=< createHead state

                HydraPayRoute_HeadBalance :/ (head, addr) -> do
                  result <- headBalance state head addr
                  writeLBS $ Aeson.encode result

                HydraPayRoute_L1Balance :/ addr -> do
                  writeLBS . Aeson.encode =<< l1Balance state addr True

                HydraPayRoute_Funds :/ addr -> do
                  writeLBS . Aeson.encode =<< getProxyFunds state addr

                HydraPayRoute_Api :/ () -> do
                  runWebSocketsSnap $ \pendingConn -> do
                    conn <- WS.acceptRequest pendingConn
                    WS.forkPingThread conn 30
                    forever $ do
                      mClientMsg <- Aeson.decode <$> WS.receiveData conn
                      case mClientMsg of
                        Just clientMsg -> handleClientMessage state clientMsg >>= WS.sendTextData conn . Aeson.encode
                        Nothing -> WS.sendTextData conn . Aeson.encode $ InvalidMessage
                  pure ()

              BackendRoute_DemoApi :/ () -> do
                  runWebSocketsSnap $ \pendingConn -> do
                    conn <- WS.acceptRequest pendingConn
                    WS.forkPingThread conn 30
                    forever $ do
                      mClientMsg <- Aeson.decode <$> WS.receiveData conn
                      case mClientMsg of
                        Just clientMsg -> WSD.handleClientMessage state cninf participants clientMsg >>= WS.sendTextData conn . Aeson.encode
                        Nothing -> WS.sendTextData conn . Aeson.encode $ InvalidMessage
                  pure ()
                
              BackendRoute_DemoAddresses :/ () -> do
                writeLBS $ Aeson.encode [addr1,addr2]
                pure ()

              BackendRoute_DemoTestWithdrawal :/ () -> do
                liftIO $ runHydraPayClient $ \conn -> do

                  Just (FundsTx tx) <- requestResponse conn $ GetAddTx Funds addr1 (ada 1000)
                  signAndSubmitTx cninf (_signingKey ks1) tx

                  Just OperationSuccess <- requestResponse conn $ Withdraw addr1
                  pure ()

              BackendRoute_Api :/ () -> pure ()
              BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }


seconds :: Int -> Int
seconds = (* 1000000)
