-- |

module HydraPay.Worker where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.DB.Beam.Types (WrapColumnar(..))
import Rhyolite.Task.Beam

import HydraPay.Database.Workers

import Prelude hiding (log)

import Cardano.Api.Extras
import Control.Concurrent (forkIO, killThread, threadDelay)
import Cardano.Transaction
import Cardano.Transaction.Eval
import Control.Lens ((^.), (^?), to, view)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Foldable
import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.IO.Temp
import Data.Time
import Data.Traversable
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import qualified Database.Beam.Postgres as Pg
import Rhyolite.Task.Beam.Worker

import HydraPay.State
import HydraPay.Bank
import HydraPay.PaymentChannel (PaymentChannelStatus(..))
import HydraPay.Cardano.Hydra.Api.ClientInput as CI
import HydraPay.Database
import qualified HydraPay.Database as Db
import HydraPay.Proxy
import HydraPay.Utils
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Hydra
import HydraPay.Cardano.Hydra.Status
import HydraPay.PaymentChannel.Postgres
import HydraPay.Transaction
import HydraPay.Types

import Control.Exception

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

paymentChannelTask :: TaskWithoutHasRun Postgres PaymentChannelTaskT (WrapColumnar PaymentChannelReq) Text (WrapColumnar (Maybe Bool))
paymentChannelTask = TaskWithoutHasRun
  { _taskWithoutHasRun_filter = \chan -> _paymentChannelTask_status chan /=. (val_ $ Just True)

  , _taskWithoutHasRun_payload = WrapColumnar . _paymentChannelTask_payload
  , _taskWithoutHasRun_checkedOutBy = _paymentChannelTask_checkedOutBy
  , _taskWithoutHasRun_result = WrapColumnar . _paymentChannelTask_status
  }

-- | Manage spawning task workers.
spawnWorkers :: Pool Pg.Connection -> HydraPayState -> IO (IO ())
spawnWorkers pool state = do
  -- clean up unfinished tasks on startup.
  Db.runBeam state cleanupCheckedOutTasks

  taskWorkerThreadId <- forkIO $ forever $ do
    t :: Either SomeException () <- try $ do
      _ <- withResource pool $ \conn -> do
        paymentChannelWorker state conn ("#" <> tShow (0::Int))
      pure ()
    threadDelay $ 1000 * 1000 * 5
    case t of
      Left err -> do
       appendFile "worker-thread.error.log" $ T.unpack errMsg
       logInfo state "Worker 0" $ "An exception occured: " <> errMsg
       where
         errMsg = tShow err
      Right _ -> pure ()

  pokeWorkerThreadId <- forkIO $ forever $ do
    logInfo state "Poke" "Waking Poke worker"
    _ <- Db.runBeam state $ do
      allHeads <- runSelectReturningList $ select $ do
        head_ <- all_ $ Db.db ^. Db.db_heads
        guard_ $ head_ ^. Db.hydraHead_status /=. val_ HydraHeadStatus_Done
        pure (head_)

      for_ allHeads $ \head_ -> do
         let
           logger = "Poke"
           log :: MonadIO m => T.Text -> m ()
           log a = logInfo state logger $ "Head " <> tShow headId <> ": " <> a
           headId = unSerial $ head_ ^. Db.hydraHead_id
           status = head_ ^. Db.hydraHead_status
         log $ "Is " <> tShow status
         case status of
           HydraHeadStatus_Created -> do
             let
               addr = unsafeToAddressAny $ head_ ^. Db.hydraHead_first
             result <- runExceptT $ do
               proxyInfo <- ExceptT $ queryProxyInfo state headId addr
               balance <- ExceptT $ runCardanoCli state $ queryUTxOs $ proxyInfo ^. proxyInfo_internalWalletAddress
               when (totalLovelace balance < ada 1) $ throwError "Node internal wallet does not yet have funds"
               ExceptT $ sendClientInputToHead state headId Nothing $ Init
               pure ()
             case result of
               Right _ -> do
                 log "Sent Init"
               Left err ->
                 log $ "Failed to send Init: " <> err

           HydraHeadStatus_Open -> do
             log "Checking to see if I should kick of a close"
             when (head_ ^. Db.hydraHead_shouldClose) $ do
               log "Signalling payment channel to close"
               result <- sendClientInputToHead state headId Nothing $ CI.Close
               case result of
                 Right _ -> do
                   log "Sent Close"
                 Left err -> do
                   log $ "Failed to send Close: " <> err

           HydraHeadStatus_Initialized -> do
             commits <- runSelectReturningList $ select $ do
               comm <- all_ $ Db.db ^. Db.db_observedCommits
               guard_ $ comm ^. Db.observedCommit_head ==. (val_ $ Db.HeadId $ head_ ^. Db.hydraHead_id)
               pure comm

             result <- runExceptT $ do
               let
                 sendCommitIfNecessary addr = do
                   theProxy <- ExceptT $ queryProxyInfo state headId addr
                   -- NOTE(skylar): When we observe a commit, the Hyrda Node reports the commit without the CBOR header which means we must drop 4 characters
                   -- furthermore the vkey we are sent is the HYDRA key and not the cardano vkey for the Proxy Address
                   value :: Aeson.Value <- ExceptT $ first (T.pack) . Aeson.eitherDecode <$> (liftIO $ LBS.readFile $ theProxy ^. proxyInfo_hydraVerificationKey)
                   vkey <- ExceptT $ pure $ maybeToEither "Failed to find key 'cborHex' in Hydra Verification key" $ (value ^? key "cborHex" . _String . to (T.drop 4))

                   let
                     commitWasObserved = any (view (observedCommit_vkey . to (==vkey))) commits
                     manager = state ^. hydraPay_httpManager

                   when (not commitWasObserved) $ do
                     log $ "Have not observed a 'Committed' for " <> tShow addr <> " yet, sending Commit"
                     let
                       theProxyAddress = theProxy ^. proxyInfo_address

                     balance <- ExceptT $ runCardanoCli state $ queryUTxOs $ theProxyAddress
                     commitUtxo <- ExceptT $ pure $ maybeToEither ("Failed to find suitable commit, waiting on fund") $ firstNonFuelUTxO balance

                     let
                       draftUtxo = massageUtxo commitUtxo

                     runningHeadVar <- ExceptT $ getRunningHead state headId
                     port <- ExceptT $ withTMVar runningHeadVar $ \rh -> do
                       ensureHeadNodesReady state rh
                       let mNode = getNodeFor rh theProxyAddress
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

                     initialRequest <- adapter $ HTTP.parseRequest $ "http://localhost:" <> show port <> "/commit"
                     let
                       request =
                         HTTP.setRequestManager manager $ initialRequest
                         { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ DraftCommitTxRequest draftUtxo } -- Aeson.encode $ DraftCommitTxRequest $ Aeson.toJSON commitUtxo

                     log "Getting draft commit from node"
                     DraftCommitTxResponse txCbor <- fmap HTTP.getResponseBody $ adapter $ HTTP.httpJSON request

                     log "Signing and submitting commit to node"
                     txid <- ExceptT $ liftIO $ withTempFile "." "commit-tx" $ \txPath txHandle -> do
                       withTempFile "." "commit-tx-signed" $ \outPath outHandle -> do
                         withTempFile "." "witness-tx" $ \witnessProxyPath witnessProxyHandle -> do
                           withTempFile "." "witness-tx" $ \witnessInternalPath witnessInternalHandle -> runExceptT $ do
                             let
                               fileData = T.intercalate "\n"
                                 [ "{"
                                 , "\"type\": \"Unwitnessed Tx BabbageEra\","
                                 , "\"description\": \"Ledger Cddl Format\","
                                 , "\"cborHex\": \"" <> txCbor <> "\""
                                 , "}"
                                 ]

                             liftIO $ do
                               hClose txHandle
                               hClose outHandle
                               hClose witnessProxyHandle
                               hClose witnessInternalHandle
                               T.writeFile txPath fileData

                             ExceptT $ runCardanoCli state $ witnessTx (theProxy ^. proxyInfo_signingKey) txPath witnessProxyPath
                             ExceptT $ runCardanoCli state $ witnessTx (theProxy ^. proxyInfo_internalWalletSigningKey) txPath witnessInternalPath
                             ExceptT $ runCardanoCli state $ assembleTx txPath [witnessProxyPath, witnessInternalPath] outPath
                             _ <- ExceptT $ runCardanoCli state $ submitTxFile outPath
                             ExceptT $ runCardanoCli state $ getTxId outPath
                     ExceptT $ waitForTxInput state $ mkTxInput txid 0
                     pure ()
                   pure commitWasObserved

               firstObs  <- sendCommitIfNecessary $ unsafeToAddressAny $ head_ ^. Db.hydraHead_first
               secondObs <- sendCommitIfNecessary $ unsafeToAddressAny $ head_ ^. Db.hydraHead_second
               when (firstObs && secondObs) $ log "We have observed both participants commits, waiting for Open"
               pure ()
             case result of
               Right _ -> pure ()
               Left err ->
                 log err
           _ -> pure ()
      pure ()
    threadDelay $ 5000000
  pure $ do
    killThread taskWorkerThreadId >> killThread pokeWorkerThreadId

-- This worker searches for refunds and creates task worker requests for them
watchRefundTasks :: MonadIO m => Pool Pg.Connection -> HydraPayState -> m (IO ())
watchRefundTasks _ state = do
  tid <- liftIO $ forkIO $ forever $ do
    now <- getCurrentTime
    _ <- Db.runBeam state $ runExceptT $ do
      refundRequests <- getExpiredPaymentChannels state now
      forM_ refundRequests $ \rr -> addPaymentChannelTask $ PaymentChannelReq_InitiatorRefund rr
    threadDelay 60000000
  pure $ killThread tid

-- | Clean who is running a tasks. Useful on app startup as no worker should be
-- running any tasks as in the case the backend restarted.
cleanupCheckedOutTasks :: MonadBeam Postgres m => m ()
cleanupCheckedOutTasks = do
  runUpdate $ update (_db_paymentChanTask db)
    (\task -> _paymentChannelTask_checkedOutBy task <-. val_ Nothing)
    (\task -> isJust_ $ _paymentChannelTask_checkedOutBy task)

-- | Insert a task for initializing paymenting a channel.
addPaymentChannelTask :: MonadBeam Postgres m => PaymentChannelReq -> m ()
addPaymentChannelTask payload = runInsert $ insert (_db_paymentChanTask db) $
  insertExpressions
    [ PaymentChannelTask
        { _paymentChannelTask_id = default_
        , _paymentChannelTask_checkedOutBy = val_ Nothing
        , _paymentChannelTask_payload = val_ payload
        , _paymentChannelTask_status = val_ Nothing
        , _paymentChannelTask_finished = val_ False
        , _paymentChannelTask_time = current_timestamp_
        }
    ]

-- | Worker responsible for Payment Channel tasks like create, open or closing
-- of channels.
paymentChannelWorker :: MonadIO m => HydraPayState -> Pg.Connection -> Text -> m Bool
paymentChannelWorker state conn workerId = taskWorkerWithoutHasRun conn (_db_paymentChanTask db) paymentChannelTask work workerId
  where
    bank = state ^. hydraPay_bank
    work _ (WrapColumnar (PaymentChannelReq_Cleanup headId)) = Db.runBeam state $ do
      result <- runExceptT $ do
        hydraHead <- getHydraHead headId
        let
          firstAddr = unsafeToAddressAny $ Db._hydraHead_first hydraHead
          secondAddr = unsafeToAddressAny $ Db._hydraHead_second hydraHead
          firstAddrStr = T.unpack $ Db._hydraHead_first hydraHead
          secondAddrStr = T.unpack $ Db._hydraHead_second hydraHead

        proxyInfoFirst <- ExceptT $ queryProxyInfo state headId firstAddr
        proxyInfoSecond <- ExceptT $ queryProxyInfo state headId secondAddr

        let
          (firstInfo, secondInfo) = (proxyInfoFirst, proxyInfoSecond)
          (firstInternalAddr, secondInternalAddr) = (_proxyInfo_internalWalletAddress firstInfo, _proxyInfo_internalWalletAddress secondInfo)
          (firstInternalAddrStr, secondInternalAddrStr) = (addressString firstInternalAddr, addressString secondInternalAddr)

        firstInternalBalance <- ExceptT $ runCardanoCli state $ queryUTxOs firstInternalAddr
        secondInternalBalance <- ExceptT $ runCardanoCli state $ queryUTxOs secondInternalAddr

        let
          firstInternalTotal = totalLovelace firstInternalBalance
          secondInternalTotal = totalLovelace secondInternalBalance

        pparams <- ExceptT $ runCardanoCli state getProtocolParameters
        _ <- ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
          let
            cfg = mkEvalConfig state paramsPath

          cleanupTx <- ExceptT $ (fmap . fmap) (TxId . T.pack) $ evalEither cfg $ do
            toFirst <- fmap oValue $ output firstAddrStr $ mkLovelaceValue $ fromIntegral $ firstInternalTotal - 1000000
            toSecond <- fmap oValue $ output secondAddrStr $ mkLovelaceValue $ fromIntegral $ secondInternalTotal - 1000000
            _ <- selectInputsAndBalance toFirst firstInternalAddrStr firstInternalAddrStr
            _ <- selectInputsAndBalance toSecond secondInternalAddrStr secondInternalAddrStr
            changeAddress firstAddrStr
            sign $ firstInfo ^. proxyInfo_internalWalletSigningKey
            sign $ secondInfo ^. proxyInfo_internalWalletSigningKey
          _ <- waitForTxInput state $ mkTxInput cleanupTx 0
          untrackRunningHead state headId
          Db.runBeam state $ do
            updateHydraHeadStatusQ headId $ HydraHeadStatus_Done
            tryUpdatePaymentChannelForHeadStatusQ headId $ PaymentChannelStatus_Done
        pure ()
      pure $ case result of
        Right _ -> do
          logInfo state "CleanupWorker" $ "Cleaned up head " <> tShow headId
          pure $ pure $ WrapColumnar $ Just True
        Left err -> do
          logInfo state "CleanupWorker" $ "Failed to cleanup: " <> err
          pure $ pure $ WrapColumnar $ Just False

    work _ (WrapColumnar (PaymentChannelReq_Fund headId requests)) = do
      result <- runExceptT $ do
        logInfo state "BankTeller" $ "Attempting to fund payment channel: " <> tShow headId

        -- First we batch transact adding the fuel to both proxies and the funds for the initiating user
        pparams <- ExceptT $ runCardanoCli state getProtocolParameters
        ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
          let
            cfg = mkEvalConfig state paramsPath
            bankStr = bank ^. bank_address . to addressString
          txid <- ExceptT $ (fmap . fmap) (TxId . T.pack) $ evalEither cfg $ do
            outputs <- fmap mconcat $ for requests $ \(FundRequest address lovelace mHash) -> do
              let value = mkLovelaceValue $ fromIntegral lovelace
              oValue <$> case mHash of
                Just hash -> outputWithDatumHash address value (BS.unpack hash)
                Nothing -> output address value

            _ <- selectInputsAndBalance outputs bankStr bankStr
            changeAddress bankStr
            sign $ bank ^. bank_signingKey
          waitForTxInput state $ mkTxInput txid 0

      pure $ case result of
        Right _ -> do
          logInfo state "BankTeller" $ "Successfully funded payment channel " <> tShow headId
          pure $ pure $ WrapColumnar $ Just True
        _ -> do
          logInfo state "BankTeller" $ "Failed to fund payment channel " <> tShow headId
          pure $ pure $ WrapColumnar $ Just False

    work _ (WrapColumnar (PaymentChannelReq_Join headId addrText amount)) = do
      result <- runExceptT $ do
        logInfo state "BankTeller" $ "Attempting to fund join to payment channel: " <> tShow headId

        -- First we batch transact adding the fuel to both proxies and the funds for the initiating user
        pparams <- ExceptT $ runCardanoCli state getProtocolParameters
        ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
          let
            cfg = mkEvalConfig state paramsPath
            bankStr = bank ^. bank_address . to addressString
          txid <- ExceptT $ (fmap . fmap) (TxId . T.pack) $ evalEither cfg $ do
            outputs <- fmap mconcat $ for [FundRequest (T.unpack addrText) (fromIntegral amount) Nothing] $ \(FundRequest address lovelace mHash) -> do
              let value = mkLovelaceValue $ fromIntegral lovelace
              oValue <$> case mHash of
                Just hash -> outputWithDatumHash address value (BS.unpack hash)
                Nothing -> output address value

            _ <- selectInputsAndBalance outputs bankStr bankStr
            changeAddress bankStr
            sign $ bank ^. bank_signingKey
          waitForTxInput state $ mkTxInput txid 0

      pure $ case result of
        Right _ -> do
          logInfo state "BankTeller" $ "Successfully funded join payment channel " <> tShow headId
          pure $ pure $ WrapColumnar $ Just True
        _ -> do
          logInfo state "BankTeller" $ "Failed to fund join payment channel " <> tShow headId
          pure $ pure $ WrapColumnar $ Just False

    work _ (WrapColumnar (PaymentChannelReq_SpinUpHead headId)) = do
      logInfo state "SpinUpHeadWorker" $ "Attempting to spin up Head " <> tShow headId
      result <- runExceptT $ do
        _ <- spinUpHead state headId
        pure ()

      case result of
        Right _ -> do
          logInfo state "SpinUpHeadWorker" $ "Head is running " <> tShow headId
          pure $ pure $ pure $ WrapColumnar $ Just True
        Left _ -> do
          logInfo state "SpinUpHeadWorker" $ "Failed to spin up head " <> tShow headId
          pure $ pure $ pure $ WrapColumnar $ Just False

    work _ (WrapColumnar (PaymentChannelReq_InitiatorRefund refundRequest)) = do
      logInfo state "Handler" "InitiatorRefund Worker: running.."
      pure $ do
        results <- handleChannelRefund (state) refundRequest
        case results of
          Left e -> do
            logInfo state "Handler" $ "InitiatorRefund Worker: error: " <> e
            pure $ pure $ (WrapColumnar (Just False) :: WrapColumnar (Maybe Bool) Identity)
          Right outcome -> do
            logInfo state "Handler" $ "InitiatorRefund Worker: Done: " <> (T.pack $ show outcome)
            pure $ pure $ (WrapColumnar (Just True) :: WrapColumnar (Maybe Bool) Identity)

    work _ (WrapColumnar (PaymentChannelReq_SubmitTxWork l1Addr tx)) = do
      logInfo state "Handler" "SubmitTx Worker: running.."
      pure $ do
        results <- handleSubmitTx (state) l1Addr tx
        case results of
          Left e -> do
            logInfo state "Handler" $ "SubmitTx Worker: error: " <> e
            pure $ pure $ (WrapColumnar (Just False) :: WrapColumnar (Maybe Bool) Identity)
          Right outcome -> do
            logInfo state "Handler" $ "SubmitTx Worker:  Done: " <> (T.pack $ show outcome)
            pure $ pure $ (WrapColumnar (Just True) :: WrapColumnar (Maybe Bool) Identity)

sendClientInputToHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Maybe ProxyAddress -> ClientInput ->  m (Either Text ())
sendClientInputToHead state headId mAddr cinput = runExceptT $ do
  runningHeadVar <- ExceptT $ getRunningHead state headId
  ExceptT $ withTMVar runningHeadVar $ \rh -> do
  sendRes <- sendClientInput rh mAddr cinput
  pure (rh, sendRes)
