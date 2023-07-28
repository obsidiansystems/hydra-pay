{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay where

import System.IO.Unsafe
import Control.Concurrent.STM
import Data.Tuple (swap)
import Text.Printf
import Cardano.Api.Extras
import HydraPay.Cardano.Hydra.Api hiding (utxo)
import HydraPay.Cardano.Hydra.Api.ClientInput
import Control.Monad.Error.Class
import HydraPay.PaymentChannel
import qualified Data.Text.Encoding as T
import HydraPay.Orphans
import qualified Cardano.Ledger.BaseTypes as Ledger
import Options.Applicative
import GHC.Generics (Generic)
import System.FilePath
import HydraPay.Path
import HydraPay.Cardano.Hydra.ChainConfig (HydraChainConfig(..))
import HydraPay.Cardano.Node (NodeConfig(..))
import Control.Exception
import System.Which
import System.Directory
import System.IO
import System.IO.Temp
import Snap.Http.Server

import qualified Data.Map as Map
import Data.Aeson.Lens
import Data.Bifunctor

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Shelley

import HydraPay.Bank
import HydraPay.PortRange
import HydraPay.Types
import HydraPay.Utils
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra
import HydraPay.Cardano.Hydra.Status
import HydraPay.Database.Workers
import HydraPay.PaymentChannel.Postgres
import HydraPay.Proxy
import HydraPay.State
import HydraPay.Worker
import HydraPay.Transaction
import qualified HydraPay.Database as Db

import Control.Concurrent (threadDelay)

import qualified Data.Aeson as Aeson
import Data.Int
import qualified Data.ByteString.Char8 as B8
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS

import Cardano.Transaction hiding (TxId, Parser)
import Cardano.Transaction.CardanoApi
import Cardano.Transaction.Extras (evalRawNoSubmit, evalRawNoSign)
import Cardano.Transaction.Eval

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

import Data.Pool
import Gargoyle.PostgreSQL.Connect
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Query

data HydraPayConfig = HydraPayConfig
  { hydraPaySettings_database :: FilePath
  , hydraPaySettings_logSettings :: LogConfig
  , hydraPaySettings_nodeConfig :: NodeConfig
  }

data BootstrapState = BootstrapState
  { _bootstrap_nodeInfo :: NodeInfo
  , _bootstrap_logger :: Logger
  }

makeLenses ''BootstrapState

instance HasLogger BootstrapState where
  getLogger = bootstrap_logger

instance HasNodeInfo BootstrapState where
  nodeInfo = bootstrap_nodeInfo

hydraChainConfig :: HydraChainConfig
hydraChainConfig =
  HydraChainConfig
  hydraChainGenesisShelley
  hydraChainProtocolParameters

previewNodeConfig :: NodeConfig
previewNodeConfig =
  NodeConfig
  previewChainConfig
  cardanoNodeDb
  (cardanoNodeDb </> "node.socket")
  previewChainTopology
  2

previewConfig :: HydraPayConfig
previewConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  previewNodeConfig

runHydraPay :: HydraPayConfig -> (HydraPayState -> IO a) -> IO a
runHydraPay (HydraPayConfig db ls ncfg) action = withLogger ls $ \l -> withDb db $ \pool -> do
  range <- mkPortRange [9000..11000]
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  withResource pool Db.doAutomigrate
  withCardanoNode ncfg $ \ni -> do
    withHydraHeadManager $ \manager -> do
      bank <- getBank $ BootstrapState ni l
      let hpstate = HydraPayState ni pool l manager httpManager range bank
      stopWorkers <- spawnWorkers pool hpstate
      stopRefund <- watchRefundTasks pool hpstate

      startupResults <- Db.runBeam hpstate $ startupExistingHeads hpstate
      case startupResults of
        Left err -> logInfo hpstate "HydraPay" $ "Failed to startup heads: " <> err
        Right 0 -> logInfo hpstate "HydraPay" $ "No existing heads found, proceeding"
        Right n -> logInfo hpstate "HydraPay" $ tShow n <> " heads are now running"
      flip finally (stopWorkers >> stopRefund) $ action $ hpstate

data InstanceRequest
  = CreatePaymentChannel Text Api.AddressAny Api.AddressAny
  | GetStatus Text
  | GetLock Text Api.AddressAny Int32
  | SendInChannel Text Api.AddressAny Int32
  | SubmitInChannel Text Api.AddressAny Text
  | CloseChannel Text
  deriving (Generic)

data InstanceResponse
  = Message Text
  | TxToSign Text
  deriving (Generic)

instance Aeson.ToJSON InstanceRequest
instance Aeson.FromJSON InstanceRequest

instance Aeson.ToJSON InstanceResponse
instance Aeson.FromJSON InstanceResponse

runPreviewInstance :: IO ()
runPreviewInstance = do
  runHydraPay previewConfig $ \state -> do
    let
      cfg = setPort 8010 $ defaultConfig
    createDirectoryIfMissing True "log"
    httpServe cfg $ WS.runWebSocketsSnap $ \pendingConn -> do
      conn <- WS.acceptRequest pendingConn
      _ <- WS.withPingThread conn 30 (pure ()) $ do
        forever $ do
          payload <- WS.receiveDataMessage conn
          let result = Aeson.eitherDecode $ case payload of
                WS.Text v _ -> v
                WS.Binary v -> v
          case result of
            Right req -> do
              res <- instanceHandler state req
              WS.sendDataMessage conn $ WS.Text (LBS.fromStrict $ T.encodeUtf8 res) Nothing
              WS.sendClose conn $ ("All Done" :: T.Text)
            Left err -> do
              logError state "Api Socket" "We received an invalid payload, failing"
              error err
      pure ()

instanceHandler :: MonadIO m => HydraPayState -> InstanceRequest -> m (Text)
instanceHandler state = \case
  CreatePaymentChannel name addr1 addr2 -> do
    result <- openPaymentChannel state name addr1 addr2
    case result of
      Right result -> pure result
      Left err -> pure err

  GetStatus name -> do
    result <- runExceptT $ getPaymentChannelAndHead state name
    case result of
      Right (chan, hhead) -> case chan ^. Db.paymentChannel_status of
        PaymentChannelStatus_Submitting -> pure $ name <> " is currently initializing, ensure you have funded an internal wallet"
        PaymentChannelStatus_WaitingForAccept -> pure $ name <> " is waiting for locked funds (do 'hydra-pay channel lock " <> name <> " <address>') to get a lock transaction"
        PaymentChannelStatus_Opening -> pure $ name <> " has locked funds, opening on L1..."
        PaymentChannelStatus_Open -> do
          details <- runExceptT $ do
            let
              pid = chan ^. Db.paymentChannel_id . to unSerial
              hid = hhead ^. Db.hydraHead_id . to unSerial
              addrFirstText = hhead ^. Db.hydraHead_first
              addrFirst = unsafeToAddressAny $ addrFirstText
              addrSecondText = hhead ^. Db.hydraHead_second
              addrSecond = unsafeToAddressAny $ addrSecondText

              addrFirstShort = T.takeEnd 8 addrFirstText
              addrSecondShort = T.takeEnd 8 addrSecondText

            (firstBalance, firstTxs) <- ExceptT $ getPaymentChannelDetails state addrFirst pid
            (secondBalance, secondTxs) <- ExceptT $ getPaymentChannelDetails state addrSecond pid

            balanceFirst <- fmap totalLovelace $ ExceptT $ getAddressUTxO state hid addrFirst
            balanceSecond <- fmap totalLovelace $ ExceptT $ getAddressUTxO state hid addrSecond

            let
              renderTxn :: TransactionInfo -> Text
              renderTxn ti = do
                T.intercalate " "
                  [ tShow (ti ^. transactionInfo_time)
                  , tShow (ti ^. transactionInfo_amount)
                  , case (ti ^. transactionInfo_direction) of
                      TransactionReceived -> addrFirstShort <>  " <- " <> addrSecondShort
                      TransactionSent -> addrFirstShort <>  " -> " <> addrSecondShort
                  ]

            pure $ T.intercalate "\n" $
              [ " --- Open Payment Channel '" <> name <> "' ---"
              , addrFirstShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral balanceFirst :: Double) / 1000000.0))
              , addrSecondShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral balanceSecond :: Double) / 1000000.0))
              ] <> (if Map.null firstTxs then [] else (["Transactions:"] <> (fmap renderTxn $ reverse $ Map.elems firstTxs)))
          case details of
            Right deets -> pure deets
            Left _ -> pure $ name <> " is open"
        PaymentChannelStatus_Closing -> pure $ name <> " is closing"
        PaymentChannelStatus_Error -> pure $ name <> " is in a failure state"

      Left err -> pure err

  GetLock name addr amount -> do
    result <- getLockTx state name addr amount
    case result of
      Right result -> pure result
      Left err -> pure err

  CloseChannel name  -> do
    result <- runExceptT $ do
      (pc, hhead) <- getPaymentChannelAndHead state name
      let
        headId = hhead ^. Db.hydraHead_id . to unSerial
        pcId = pc ^. Db.paymentChannel_id . to unSerial
      Db.runBeam state $ do
        runUpdate $ update (Db.db ^. Db.db_heads)
          (\head_ -> head_ ^. Db.hydraHead_shouldClose <-. val_ True)
          (\head_ -> head_ ^. Db.hydraHead_id ==. (val_ $ SqlSerial $ headId))
        updatePaymentChannelStatusQ pcId $ PaymentChannelStatus_Closing
    case result of
      Right _ -> pure $ "Closing '" <> name <> "'"
      Left err -> pure err

  SubmitInChannel name addr payload -> do
    let
      getTxJson :: MonadError Text m => m Aeson.Value
      getTxJson =
        case Aeson.eitherDecode $ LBS.fromStrict $ T.encodeUtf8 payload of
          Right (x :: Aeson.Value) ->
            case x ^? key "cborHex" of
              Just hex -> pure hex
              Nothing -> throwError "Invalid transaction"
          Left err -> throwError $ T.pack err
    result <- runExceptT $ do
      txJson <- getTxJson
      (pc, hydraHead) <- getPaymentChannelAndHead state name
      let
        hid = hydraHead ^. Db.hydraHead_id . to unSerial

      balanceBefore <- ExceptT $ getAddressUTxO state hid addr
      let
        beforeLovelace = totalLovelace balanceBefore

      runningHeadVar <- ExceptT $ getRunningHead state hid
      chan <- ExceptT $ withTMVar runningHeadVar $ \rh -> do
        result <- runExceptT $ do
          node <- ExceptT $ pure $ getNodeFor' rh $ ProxyAddress addr
          output' <- liftIO $ atomically $ dupTChan $ node ^. hydraNode_outputs
          ExceptT $ sendClientInput rh (Just $ ProxyAddress addr) $ NewTx $ txJson
          pure output'
        pure (rh, result)
      ExceptT $ liftIO $ do
        let
          doThing = do
           o <- atomically $ readTChan chan
           case o of
             TxInvalid _ _ _ verr  -> pure $ Left $ "Transaction invalid: " <> tShow verr
             CommandFailed (NewTx _) -> pure $ Left "Transaction failed to submit"
             SnapshotConfirmed _ _ _ -> pure $ Right ()
             InvalidInput _ input -> pure $ Left "Invalid input expected transaction"
             _ -> doThing
        doThing
      balanceAfter <- ExceptT $ getAddressUTxO state hid addr
      let
        afterLovelace = totalLovelace balanceAfter
      ExceptT $ Db.runBeam state $ sendAdaInChannel (hydraHead ^. Db.hydraHead_id . to unSerial) addr (fromIntegral $ beforeLovelace - afterLovelace)
    case result of
      Right _ -> pure "Send successful"
      Left err -> pure err

  SendInChannel name fromAddr amount -> do
    sendResult <- Db.runBeam state $ runExceptT $ do
      (chan, hydraHead) <- getPaymentChannelAndHead state name
      let
        cfg = mkEvalConfig state hydraChainProtocolParameters
        toAddr = if fromAddr == unsafeToAddressAny (Db._hydraHead_first hydraHead)
          then Db._hydraHead_second hydraHead
          else Db._hydraHead_first hydraHead

        toAddrStr = T.unpack toAddr
        fromAddrStr = addressString fromAddr

        toAddrShort = T.takeEnd 8 $ T.pack toAddrStr
        fromAddrShort = T.takeEnd 8 $ T.pack fromAddrStr

        amountText = T.pack $ printf "%.2f" $ (fromIntegral amount :: Double) / 1000000.0

        hid = hydraHead ^. Db.hydraHead_id . to unSerial

      -- utxo <- ExceptT $ do
      --   fileData <- liftIO $ T.readFile "utxo.json"
      --   pure $ bimap T.pack id $ Aeson.eitherDecode (LBS.fromStrict $ T.encodeUtf8 $ fileData)
       --let utxo = Api.UTxO $ Map.fromList [(Shelley.TxIn "c7c4d09d2bff597da4d455725f0b5df813369f12b60d9453605486316ef29be2" (Shelley.TxIx 1),Shelley.TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "043992862e5cbefa7fcf3734c61ec890d7fbba5218be8c805d23bb83")) StakeRefNull)) (TxOutValue MultiAssetInBabbageEra (valueFromList [(AdaAssetId,100000000)])) TxOutDatumNone ReferenceScriptNone)]
      utxo <- ExceptT $ getAddressUTxO state hid fromAddr
      -- liftIO $ T.writeFile "utxo.json" $ T.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ utxo
      (_, txPayload) <- liftIO $ evalRawNoSign cfg 0 $ do
        sendHydraLovelaceTx utxo fromAddrStr toAddrStr (fromIntegral amount)

      pure $ T.intercalate "\n" [ "Here is a transaction to send " <> amountText <> "ADA from " <> fromAddrShort <> " to " <> toAddrShort <> " in your payment channel"
                                , T.decodeUtf8 $ LBS.toStrict $ txPayload
                                ]

          -- let
          --   pid = chan ^. Db.paymentChannel_id . to unSerial
          --   hid = hydraHead ^. Db.hydraHead_id . to unSerial
          --
          -- proxyInfoFirst <- ExceptT $ queryProxyInfo state hid $ unsafeToAddressAny $ Db._hydraHead_first hydraHead
          -- proxyInfoSecond <- ExceptT $ queryProxyInfo state hid $ unsafeToAddressAny $ Db._hydraHead_second hydraHead
          --
          -- let
          --   (fromProxyInfo, toProxyInfo) =
          --     (if addr == unsafeToAddressAny (Db._hydraHead_first hydraHead) then id else swap)
          --       (proxyInfoFirst, proxyInfoSecond)
          --   (fromAddr, toAddr) = (_proxyInfo_address fromProxyInfo, _proxyInfo_address toProxyInfo)
          --   (fromAddrStr, toAddrStr) = (Api.serialiseAddress fromAddr, Api.serialiseAddress toAddr)
          --
          -- utxo <- ExceptT $ getAddressUTxO state hid fromAddr
          -- let
          --   cfg = mkEvalConfig state hydraChainProtocolParameters
          -- (txId, txLbs) <- liftIO $ evalRawNoSubmit cfg 0 $ do
          --   sendHydraLovelaceTx utxo (T.unpack fromAddrStr) (T.unpack toAddrStr) (fromIntegral amount)
          --   sign (fromProxyInfo ^. proxyInfo_signingKey)
          -- (_, txCbor) <- case txLbs ^? key "cborHex" . _String of
          --   Nothing -> throwError $ "Could not decode cardano tx creation"
          --   Just cbor -> do
          --     pure (txId, cbor)
          -- logInfo state "SendInChannel" $ "Created Send TX: " <> T.pack txId
          --
          -- let
          --   txJson = Aeson.String $ txCbor
          --
          -- runningHeadVar <- ExceptT $ getRunningHead state hid
          -- chan <- ExceptT $ withTMVar runningHeadVar $ \rh -> do
          --   result <- runExceptT $ do
          --     node <- ExceptT $ pure $ maybeToEither "Failed to get node" $ getNodeFor rh fromAddr
          --     output' <- liftIO $ atomically $ dupTChan $ node ^. hydraNode_outputs
          --     ExceptT $ sendClientInput rh (Just fromAddr) $ NewTx txJson
          --     pure output'
          --   pure (rh, result)
          --
          -- ExceptT $ liftIO $ atomically $ do
          --   o <- readTChan chan
          --   case o of
          --     TxValid _ _ -> pure $ Right ()
          --     TxInvalid _ _ _ _  -> pure $ Left "NewTx: Transaction invalid"
          --     CommandFailed (NewTx _) -> pure $ Left "NewTx: Command Failed"
          --     _ -> retry
          --
          -- logInfo state "SendInChannel" $ "Sent in " <> tShow hid
          -- (balance, info) <- ExceptT $ sendAdaInChannel hid addr amount
          -- let
          --   addrFirstText = hydraHead ^. Db.hydraHead_first
          --   addrSecondText = hydraHead ^. Db.hydraHead_second
          --   addrFirstShort = T.takeEnd 8 addrFirstText
          --   addrSecondShort = T.takeEnd 8 addrSecondText
          -- pure $ "I don't work yet"
            -- T.intercalate "\n" $
            -- [ "Sent " <> (T.pack $ printf "%.2f" $ ((fromIntegral firstBalance :: Double) / 1000000.0))
            --   --" --- Payment Channel " <> name <> " ---"
            -- -- , addrFirstShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral firstBalance :: Double) / 1000000.0))
            -- -- , addrSecondShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral secondBalance :: Double) / 1000000.0))
            -- ] <> (if Map.null firstTxs then [] else (["Transactions:"] <> (fmap renderTxn $ Map.elems firstTxs)))
    case sendResult of
      Right res -> pure res
      Left res -> pure res

data Command
  = Instance
  | Channel ChannelCommand
  deriving (Show)

data ChannelCommand
  = Open Text Text Text
  | Status Text
  | Lock Text Double Text
  | Send Text Double Text
  | Submit Text Text FilePath
  | DoClose Text
  deriving (Show)

parseChannelCommand :: Parser Command
parseChannelCommand = fmap Channel $ subparser $
  command "status" (info parseStatus (progDesc "Get the status of a payment channel"))
  <> command "open" (info parseOpen (progDesc "Open a new payment channel"))
  <> command "lock" (info parseLock (progDesc "Lock funds in a payment channel"))
  <> command "send" (info parseSend (progDesc "Send ADA in a payment channel"))
  <> command "submit" (info parseSubmit (progDesc "Submit a signed transaction received from 'send'"))
  <> command "close" (info parseClose (progDesc "Close a payment channel"))

parseClose :: Parser ChannelCommand
parseClose = DoClose <$> argument str (metavar "<name>")

parseSubmit :: Parser ChannelCommand
parseSubmit = Submit <$> argument str (metavar "<name>") <*> argument str (metavar "<address>") <*> argument str (metavar "<file>")

parseSend :: Parser ChannelCommand
parseSend = Send <$> argument str (metavar "<name>") <*> argument auto (metavar "<amount-lovelace>") <*> argument str (metavar "<to-address>")

parseLock :: Parser ChannelCommand
parseLock = Lock <$> argument str (metavar "<name>") <*> argument auto (metavar "<amount-lovelace>") <*> argument str (metavar "<address>")

parseStatus :: Parser ChannelCommand
parseStatus = Status <$> argument str (metavar "<name>")

parseOpen :: Parser ChannelCommand
parseOpen = Open <$> argument str (metavar "<name>") <*> argument str (metavar "<address>") <*> argument str (metavar "<address>")

parseCommand :: Parser Command
parseCommand = subparser $
  command "instance" (info (pure Instance) (progDesc "Start a HydraPay instance"))
  <> command "channel" (info parseChannelCommand (progDesc "Payment channel commands"))

mkCreate :: Text -> Text -> Text -> Either Text InstanceRequest
mkCreate name str1 str2 = do
  addr1 <- toAddressAny str1
  addr2 <- toAddressAny str2
  pure $ CreatePaymentChannel name addr1 addr2

mkSend :: Text -> Double -> Text -> Either Text InstanceRequest
mkSend name amount str = do
  addr <- toAddressAny str
  when (amount <= 0) $ throwError "You can't lock negative ADA"
  pure $ SendInChannel name addr lovelace
  where
    lovelace :: Int32
    lovelace = round $ amount * 1000000

mkLock :: Text -> Double -> Text -> Either Text InstanceRequest
mkLock name amount str = do
  addr <- toAddressAny str
  when (amount <= 0) $ throwError "You can't lock negative ADA"
  when (amount > 100) $ throwError "You can't lock more than 100 ADA"
  pure $ GetLock name addr lovelace
  where
    lovelace :: Int32
    lovelace = round $ amount * 1000000

mkSubmit :: (MonadError Text m, MonadIO m) => Text -> FilePath -> Text -> m InstanceRequest
mkSubmit name file str = do
  exists <- liftIO $ doesFileExist file
  addr <- case addrResult of
    Right addr -> pure addr
    Left err -> throwError err
  case exists of
    False -> throwError $ "Couldn't find signed transaction file " <> T.pack file
    True -> do
      fileData <- liftIO $ T.readFile file
      pure $ SubmitInChannel name addr fileData

  where
    addrResult = toAddressAny str

runClient :: IO ()
runClient = do
  let opts = info (parseCommand <**> helper) fullDesc
  cmd <- customExecParser (prefs showHelpOnEmpty) opts
  case cmd of
    Instance -> runPreviewInstance
    Channel (Open name addrFirst addrSecond) -> do
      case mkCreate name addrFirst addrSecond of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Lock name addr amount) -> do
      case mkLock name addr amount of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Status name) -> clientRequest $ GetStatus name
    Channel (Send name addr amount) ->
      case mkSend name addr amount of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Submit name addr file) -> do
      result <- runExceptT $ mkSubmit name file addr
      case result of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (DoClose name) ->
      clientRequest (CloseChannel name)
    x -> error $ "Got unhandled " <> show x

clientRequest :: InstanceRequest -> IO ()
clientRequest req = do
  tryRes :: (Either SomeException ()) <- try $ WS.runClient "localhost" 8010 "/" $ \conn -> do
    WS.sendDataMessage conn $ WS.Text (Aeson.encode req) Nothing
    payload <- WS.receiveDataMessage conn
    WS.sendClose conn $ ("We are done here" :: T.Text)
    let
      result = case payload of
        WS.Text v _ -> v
        WS.Binary v -> v
    LBS.putStrLn result
  case tryRes of
    Left reason -> putStrLn $ "Failed to connect to HydraPay instance at " <> show 8010 <> " " <> show reason
    _ -> pure ()
  pure ()

-- | Get the network for a shelley address
getShelleyAddrNetwork :: Api.AddressAny -> Maybe Ledger.Network
getShelleyAddrNetwork = \case
  Api.AddressShelley (Shelley.ShelleyAddress network _ _) -> Just network
  Api.AddressByron _ -> Nothing

toAddressAny :: Text -> Either Text Api.AddressAny
toAddressAny a = case Api.deserialiseAddress Api.AsAddressAny (T.strip a) of
  Nothing -> Left "Invalid Address"
  Just addrAny -> return addrAny

toAddressAnyInNetwork :: Ledger.Network -> Text -> Either Text Api.AddressAny
toAddressAnyInNetwork network a = do
  addr <- first (const invalidAddrMsg) $ toAddressAny a
  case getShelleyAddrNetwork addr of
    Nothing -> Left "Invalid era"
    Just addrNetwork -> do
      when (addrNetwork /= network) $ Left invalidAddrMsg
      pure addr
  where
    invalidAddrMsg = T.unwords ["Invalid", T.pack (show network), "Address"]

getLockTx :: MonadIO m => HydraPayState -> Text -> Api.AddressAny -> Int32 -> m (Either Text Text)
getLockTx state name addr amount = runExceptT $ do
  let
    manager = state ^. hydraPay_httpManager
    addrStr = addressString addr
  pc <- ExceptT $ getPaymentChannel state name Nothing

  headId <- Db.runBeam state $ getPaymentChannelHeadId $ pc ^. Db.paymentChannel_id . to unSerial
  proxyInfo <- ExceptT $ Db.runBeam state $ queryProxyInfo state headId addr
  let
    theProxyAddress = ProxyAddress addr

  runningHeadVar <- ExceptT $ getRunningHead state headId
  port <- ExceptT $ withTMVar runningHeadVar $ \rh -> do
    ensureHeadNodesReady state rh
    let mNode = getNodeFor rh $ ProxyAddress addr -- proxyInfo ^. proxyInfo_address
    result <- case mNode of
      Just node -> do
        pure $ Right $ node ^. hydraNode_apiPort
      Nothing ->
        pure $ Left "Failed to send client input, unable to locate suitable node"
    pure (rh, result)

  let
    adapter :: (MonadError Text m, MonadIO m) => IO a -> m a
    adapter action = do
      result <- liftIO $ try $ action
      case result of
        Left (err :: SomeException) -> throwError $ tShow err
        Right a -> pure a


  internalWalletBalance <- ExceptT $ runCardanoCli state $ queryUTxOs $ proxyInfo ^. proxyInfo_internalWalletAddress
  pparams <- ExceptT $ runCardanoCli state getProtocolParameters

  case totalLovelace internalWalletBalance > ada 10 of
    False -> do
      let
        fuelAmount :: Integer
        fuelAmount = 30000000
        internalAddrStr = proxyInfo ^. proxyInfo_internalWalletAddress . to addressString
      txPayload <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
        let cfg = mkEvalConfig state paramsPath
        ExceptT $ evalTxEither cfg $ do
          out <- output internalAddrStr $ fromString $ show fuelAmount <> " lovelace"
          void $ selectInputs (oValue out) addrStr
          changeAddress addrStr
          void $ balanceNonAdaAssets addrStr
      pure $ T.intercalate "\n" [ "Please fund your Hydra Node's internal wallet, the address is " <> T.pack internalAddrStr
                                , "enclosed is a transaction that you can sign and submit that will fund this internal wallet with 30ADA"
                                , T.pack txPayload
                                ]

    True -> do
      balance <- ExceptT $ runCardanoCli state $ queryUTxOs $ theProxyAddress
      let
        amountText = T.pack $ printf "%.2f" $ (fromIntegral amount :: Double) / 1000000.0
        fullLovelace = totalLovelace balance
      case findUTxOWithExactly (fromIntegral amount) balance of
        Nothing -> do
          case fullLovelace >= fromIntegral amount of
            False ->
              pure $ "This address doesn't have enough at least " <> amountText <> "ADA to lock."
            True -> do
              txPayload <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
                let cfg = mkEvalConfig state paramsPath
                ExceptT $ evalTxEither cfg $ do
                  out <- output addrStr $ fromString $ show amount <> " lovelace"
                  void $ selectInputs (oValue out) addrStr
                  changeAddress addrStr
                  void $ balanceNonAdaAssets addrStr

              pure $ T.intercalate "\n"
                [ "This address doesn't have a commit with exactly " <> amountText <> "ADA"
                , "Here is a transaction that will create a UTXO with exactly " <> amountText <> " submit this, then run this command again."
                , "(If you recently submitted such a transaction please wait for it to appear on-chain)"
                , T.pack txPayload
                ]

        Just commitUtxo -> do
          let
            draftUtxo = massageUtxo commitUtxo
          initialRequest <- adapter $ HTTP.parseRequest $ "http://localhost:" <> show port <> "/commit"
          let
            request =
              HTTP.setRequestManager manager $ initialRequest
              { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ DraftCommitTxRequest draftUtxo }

          DraftCommitTxResponse txCbor <- fmap HTTP.getResponseBody $ adapter $ HTTP.httpJSON request
          proxyInfo <- ExceptT $ Db.runBeam state $ queryProxyInfo state headId addr
          let
             fileData = T.intercalate "\n"
               [ "{"
               , "    \"type\": \"Unwitnessed Tx BabbageEra\","
               , "    \"description\": \"Ledger Cddl Format\","
               , "    \"cborHex\": \"" <> txCbor <> "\""
               , "}"
               ]
          internalWalletWitness <- ExceptT $ liftIO $ withTempFile "." "tx" $ \txPath txHandle -> do
            withTempFile "." "witness-tx" $ \witnessPath witnessHandle -> runExceptT $ do
              liftIO $ hClose txHandle >> hClose witnessHandle >> T.writeFile txPath fileData
              ExceptT $ runCardanoCli state $ witnessTx (proxyInfo ^. proxyInfo_internalWalletSigningKey) txPath witnessPath
              liftIO $ T.readFile witnessPath

          pure $ T.intercalate "\n"
            [ "Here is your Lock transaction for this payment channel is, submit this to lock  " <> amountText <> " ADA into this payment channel"
            , ""
            , fileData
            , ""
            , "Here is the witness from the internal wallet, you may need this depending on how you sign and submit transactions:"
            , internalWalletWitness
            ]

openPaymentChannel :: MonadIO m => HydraPayState -> Text -> Api.AddressAny -> Api.AddressAny -> m (Either Text Text)
openPaymentChannel state name you other = do
  let amount = 100000000
  Db.runBeam state $ runExceptT $ do
    logInfo state "Handler" "Submitting Open Channel Tx to Worker"

    logInfo state "Handler" "Storing Payment Channel request data"
    Db.HeadId (SqlSerial headId) <- createPaymentChannel state $
      PaymentChannelConfig
      name
      you
      other
      amount
      hydraChainConfig
      False

    proxyFirst <- ExceptT $ queryProxyInfo state headId you
    proxySecond <- ExceptT $ queryProxyInfo state headId other

    pparams <- ExceptT $ runCardanoCli state getProtocolParameters

    let
      youStr = addressString you
      internalFirstAddrStr = proxyFirst ^. proxyInfo_internalWalletAddress . to addressString
    let
      fuelAmount :: Integer
      fuelAmount = 30000000
    txPayload <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
      let cfg = mkEvalConfig state paramsPath
      ExceptT $ evalTxEither cfg $ do
        out <- output internalFirstAddrStr $ fromString $ show fuelAmount <> " lovelace"
        void $ selectInputs (oValue out) youStr
        changeAddress youStr
        void $ balanceNonAdaAssets youStr

    addPaymentChannelTask $ PaymentChannelReq_SpinUpHead headId

    pure $ T.intercalate "\n" [ "New payment channel '" <> name <> "' created successfully"
                              , "please fund your Hydra Node's internal wallet, the address is " <> T.pack internalFirstAddrStr
                              , "enclosed is a transaction that you can sign and submit that will fund this internal wallet with 30ADA"
                              , T.pack txPayload
                              ]

getPaymentChannelAndHead :: (MonadIO m, MonadError Text m) => HydraPayState -> Text -> m (Db.PaymentChannel, Db.HydraHead)
getPaymentChannelAndHead state name = do
  result <- Db.runBeam state $ do
    runSelectReturningOne $ select $ do
      pc_ <- all_ $ Db.db ^. Db.db_paymentChannels
      head_ <- all_ $ Db.db ^. Db.db_heads
      guard_ ((pc_ ^. Db.paymentChannel_head) `references_` head_)
      guard_ $ pc_ ^. Db.paymentChannel_name ==. val_ name
      pure (pc_, head_)
  case result of
    Nothing -> throwError $ "Payment channel " <> name <> " not found"
    Just x -> pure x

getPaymentChannel :: MonadIO m => HydraPayState -> Text -> Maybe Api.AddressAny -> m (Either Text Db.PaymentChannel)
getPaymentChannel state name mAddr = do
  result <- Db.runBeam state $ do
    runSelectReturningOne $ select $ do
      pc_ <- all_ $ Db.db ^. Db.db_paymentChannels
      guard_ $ pc_ ^. Db.paymentChannel_name ==. val_ name
      pure pc_
  case result of
    Just c -> pure $ Right c
    Nothing -> pure $ Left $ "No payment channel '" <> name <> "' seen by this instance"

nominalFuel :: Api.Lovelace
nominalFuel = 100000000

topUpLevel :: Api.Lovelace
topUpLevel = 60000000
