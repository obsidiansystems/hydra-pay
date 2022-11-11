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

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Common.Helpers
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

setupDemo :: State -> IO ([(KeyPair, Address)])
setupDemo state = do
  cns <- readMVar (_state_cardanoNodeState state)
  case cardanoNodeState_nodeType cns of
    CfgDevnet -> do
      prefix <- liftIO getTempPath'
      oneKs <- withLogging $ generateCardanoKeys $ prefix <> "one"
      one <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey oneKs
      twoKs <- withLogging $ generateCardanoKeys $ prefix <> "two"
      two <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey twoKs
      withLogging $ do
        seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys one (ada 10000) False
        seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys two (ada 10000) False
      -- seedTestAddresses (_hydraCardanoNodeInfo hydraSharedInfo) devnetFaucetKeys 10
      pure [(oneKs, one), (twoKs, two)]

    CfgPreview pcfg -> do
      forM (_previewParticipants pcfg) $ \kp -> do
        addr <- liftIO $ getCardanoAddress (_previewNodeInfo pcfg) (_verificationKey kp)
        pure (kp, addr)

runLogging = flip runLoggingT (print . renderWithSeverity id)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- NOTE(skylar): Running heads is a map from head name to network handle
      flip runLoggingT (print . renderWithSeverity id) $ do
        liftIO $ runHydraPay $ \state -> do
          hsi <- getHydraSharedInfo state
          participants <- setupDemo state
          let cninf = _hydraCardanoNodeInfo hsi
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
                  WS.sendTextData conn . Aeson.encode $ ServerHello versionStr

                  forever $ do
                    mClientMsg <- Aeson.decode <$> WS.receiveData conn
                    case mClientMsg of
                      Just clientMsg -> do
                        msg <- handleTaggedMessage conn state clientMsg
                        WS.sendTextData conn . Aeson.encode $ msg
                      Nothing -> WS.sendTextData conn . Aeson.encode $ InvalidMessage

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

            BackendRoute_Api :/ () -> pure ()
            BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
