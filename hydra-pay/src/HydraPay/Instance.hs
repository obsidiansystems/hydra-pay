{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module HydraPay.Instance where

import Control.Concurrent.STM
import Text.Printf
import Cardano.Api.Extras
import HydraPay.Cardano.Hydra.Api hiding (utxo)
import Control.Monad.Error.Class
import HydraPay.PaymentChannel
import qualified Data.Text.Encoding as T
import HydraPay.Path
import Control.Exception
import System.Directory
import System.IO
import System.IO.Temp
import Snap.Http.Server

import Data.Aeson.Lens

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Cardano.Api as Api

import HydraPay.Bank
import HydraPay.PortRange
import HydraPay.Types
import HydraPay.Utils
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra
import HydraPay.Database.Workers
import HydraPay.PaymentChannel.Postgres
import HydraPay.Proxy
import HydraPay.State
import HydraPay.Worker
import HydraPay.Transaction
import qualified HydraPay.Database as Db

import qualified Data.Aeson as Aeson
import Data.Int
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS

import Cardano.Transaction hiding (TxId, Parser)
import Cardano.Transaction.Extras (evalRawNoSign)
import Cardano.Transaction.Eval

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

import Data.Pool
import Gargoyle.PostgreSQL.Connect
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Query

import HydraPay.Api
import HydraPay.Config

defaultPort :: Int
defaultPort = 8010

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

runInstance :: HydraPayConfig -> IO ()
runInstance cfg = do
  runHydraPay cfg $ \state -> do
    let
      networkCfg = setPort defaultPort $ defaultConfig
    createDirectoryIfMissing True "log"
    httpServe networkCfg $ WS.runWebSocketsSnap $ \pendingConn -> do
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
              WS.sendDataMessage conn $ WS.Text (Aeson.encode res) Nothing
              WS.sendClose conn $ ("All Done" :: T.Text)
            Left err -> do
              logError state "Api Socket" "We received an invalid payload, failing"
              error err
      pure ()

instanceHandler :: MonadIO m => HydraPayState -> InstanceRequest -> m InstanceResponse
instanceHandler state = \case
  CreatePaymentChannel name addr1 addr2 -> do
    result <- openPaymentChannel state name addr1 addr2
    case result of
      Right chan -> pure $ NewPaymentChannel name chan
      Left err -> pure $ InstanceError err

  GetStatus name -> do
    result <- runExceptT $ getPaymentChannelAndHead state name
    case result of
      Right (chan, hhead) -> do
        mDetails <- case status of
          PaymentChannelStatus_Open -> getDetails
          _ -> pure Nothing

        pure $ ChannelStatus name status mDetails
        where
          status = chan ^. Db.paymentChannel_status
          getDetails = do
            deetsResult <- runExceptT $ do
              let
                pid = chan ^. Db.paymentChannel_id . to unSerial
                hid = hhead ^. Db.hydraHead_id . to unSerial
                addrFirstText = hhead ^. Db.hydraHead_first
                addrSecondText = hhead ^. Db.hydraHead_second
                addrFirst = unsafeToAddressAny $ addrFirstText
                addrSecond = unsafeToAddressAny $ addrSecondText

              (_, firstTxs) <- ExceptT $ getPaymentChannelDetails state addrFirst pid
              (_, _) <- ExceptT $ getPaymentChannelDetails state addrSecond pid

              balanceFirst <- fmap totalLovelace $ ExceptT $ getAddressUTxO state hid addrFirst
              balanceSecond <- fmap totalLovelace $ ExceptT $ getAddressUTxO state hid addrSecond

              pure $ DetailedStatus addrFirst addrSecond (fromIntegral balanceFirst) (fromIntegral balanceSecond) firstTxs
            case deetsResult of
              Right deets -> pure $ Just deets
              Left _ -> pure $ Nothing

      Left err -> pure $ InstanceError err

  GetLock name addr amount -> do
    result <- getLockTx state name addr amount
    case result of
      Right lock -> pure $ LockAttempt name lock
      Left err -> pure $ InstanceError err

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
      Right _ -> pure $ ChannelStatus name PaymentChannelStatus_Closing Nothing
      Left err -> pure $ InstanceError err

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
      (_, hydraHead) <- getPaymentChannelAndHead state name
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
             InvalidInput _ _ -> pure $ Left "Invalid input expected transaction"
             _ -> doThing
        doThing
      balanceAfter <- ExceptT $ getAddressUTxO state hid addr
      let
        afterLovelace = totalLovelace balanceAfter
      ExceptT $ Db.runBeam state $ sendAdaInChannel (hydraHead ^. Db.hydraHead_id . to unSerial) addr (fromIntegral $ beforeLovelace - afterLovelace)
    case result of
      Right _ -> pure $ SuccessMessage "Send successful"
      Left err -> pure $ InstanceError err

  SendInChannel name fromAddr amount -> do
    sendResult <- Db.runBeam state $ runExceptT $ do
      (_, hydraHead) <- getPaymentChannelAndHead state name
      let
        cfg = mkEvalConfig state hydraChainProtocolParameters
        toAddr = if fromAddr == unsafeToAddressAny (Db._hydraHead_first hydraHead)
          then Db._hydraHead_second hydraHead
          else Db._hydraHead_first hydraHead

        toAddrStr = T.unpack toAddr
        fromAddrStr = addressString fromAddr

        hid = hydraHead ^. Db.hydraHead_id . to unSerial

      utxo <- ExceptT $ getAddressUTxO state hid fromAddr
      (_, txPayload) <- liftIO $ evalRawNoSign cfg 0 $ do
        sendHydraLovelaceTx utxo fromAddrStr toAddrStr (fromIntegral amount)

      pure $ SendTxRaw fromAddr (unsafeToAddressAny toAddr) amount (T.decodeUtf8 $ LBS.toStrict $ txPayload)

    case sendResult of
      Right rawTx -> pure $ SendTx name rawTx
      Left res -> pure $ InstanceError res

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
getPaymentChannel state name _ = do
  result <- Db.runBeam state $ do
    runSelectReturningOne $ select $ do
      pc_ <- all_ $ Db.db ^. Db.db_paymentChannels
      guard_ $ pc_ ^. Db.paymentChannel_name ==. val_ name
      pure pc_
  case result of
    Just c -> pure $ Right c
    Nothing -> pure $ Left $ "No payment channel '" <> name <> "' seen by this instance"

getLockTx :: MonadIO m => HydraPayState -> Text -> Api.AddressAny -> Int32 -> m (Either Text LockResult)
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
        internalAddr = proxyInfo ^. proxyInfo_internalWalletAddress
        internalAddrStr = proxyInfo ^. proxyInfo_internalWalletAddress . to addressString
      txPayload <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
        let cfg = mkEvalConfig state paramsPath
        ExceptT $ evalTxEither cfg $ do
          out <- output internalAddrStr $ fromString $ show fuelAmount <> " lovelace"
          void $ selectInputs (oValue out) addrStr
          changeAddress addrStr
          void $ balanceNonAdaAssets addrStr
      pure $ LockFundInternal $ FundInternalWallet internalAddr $ T.pack txPayload

    True -> do
      balance <- ExceptT $ runCardanoCli state $ queryUTxOs $ theProxyAddress
      let
        amountText = T.pack $ printf "%.2f" $ (fromIntegral amount :: Double) / 1000000.0
        fullLovelace = totalLovelace balance
      case amount > 0 of
        False -> do
          let
            draftUtxo = mempty
          initialRequest <- adapter $ HTTP.parseRequest $ "http://127.0.0.1:" <> show port <> "/commit"
          let
            request =
              HTTP.setRequestManager manager $ initialRequest
              { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ DraftCommitTxRequest draftUtxo }

          DraftCommitTxResponse txCbor <- fmap HTTP.getResponseBody $ adapter $ HTTP.httpJSON request
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

          pure $ LockInChannel $ FundPaymentChannel amount txCbor internalWalletWitness
        True ->
          case findUTxOWithExactly (fromIntegral amount) balance of
            Nothing -> do
              case fullLovelace >= fromIntegral amount of
                False ->
                  throwError $ "This address doesn't have enough at least " <> amountText <> "ADA to lock."
                True -> do
                  txPayload <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
                    let cfg = mkEvalConfig state paramsPath
                    ExceptT $ evalTxEither cfg $ do
                      out <- output addrStr $ fromString $ show amount <> " lovelace"
                      void $ selectInputs (oValue out) addrStr
                      changeAddress addrStr
                      void $ balanceNonAdaAssets addrStr

                  pure $ LockCreateOutput amount $ T.pack txPayload

            Just commitUtxo -> do
              let
                draftUtxo = massageUtxo commitUtxo
              initialRequest <- adapter $ HTTP.parseRequest $ "http://127.0.0.1:" <> show port <> "/commit"
              let
                request =
                  HTTP.setRequestManager manager $ initialRequest
                  { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ DraftCommitTxRequest draftUtxo }

              DraftCommitTxResponse txCbor <- fmap HTTP.getResponseBody $ adapter $ HTTP.httpJSON request
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

              pure $ LockInChannel $ FundPaymentChannel amount txCbor internalWalletWitness

openPaymentChannel :: MonadIO m => HydraPayState -> Text -> Api.AddressAny -> Api.AddressAny -> m (Either Text FundInternalWallet)
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

    pparams <- ExceptT $ runCardanoCli state getProtocolParameters

    let
      youStr = addressString you
      internalAddress = proxyFirst ^. proxyInfo_internalWalletAddress
      internalFirstAddrStr = internalAddress ^. to addressString
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

    pure $ FundInternalWallet internalAddress $ T.pack txPayload
