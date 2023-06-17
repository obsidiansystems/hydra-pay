{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra where

import Prelude hiding (log)
import Debug.Trace (traceM, traceShow)

import System.IO
import System.Which
import System.Process
import System.FilePath
import System.Directory

import Data.Int
import Data.Word
import Data.Maybe
import Data.Text (Text)
import Data.Bool (bool)
import Data.Foldable
import Data.Traversable
import Data.Aeson (encode, eitherDecode)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Proxy
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Hydra.ChainConfig (HydraChainConfig(..))
import HydraPay.Cardano.Node
import HydraPay.PaymentChannel (PaymentChannelStatus(..))
import HydraPay.PaymentChannel.Postgres (updatePaymentChannelStatusQ)
import HydraPay.PortRange
import HydraPay.Cardano.Hydra.RunningHead
import HydraPay.Cardano.Hydra.Api hiding (headId, headStatus)
import HydraPay.Transaction
import qualified HydraPay.Database as Db

import qualified Cardano.Api as Api

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions

import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LBS


import Network.WebSockets (runClient, sendDataMessage, withPingThread)
import qualified Network.WebSockets as WS

data HydraHeadManager = HydraHeadManager
  { _hydraHeadManager_runningHeads :: TMVar (Map Int32 (TMVar RunningHydraHead))
  }

class HasHydraHeadManager a where
  hydraHeadManager :: Lens' a HydraHeadManager

instance HasHydraHeadManager HydraHeadManager where
  hydraHeadManager = id

data HydraHeadInput :: * -> * where
  HydraHeadInit :: HydraHeadInput (Either Text ())
  HydraHeadCommit :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadInput (Either Text ())
  HydraHeadGetAddressUTxO :: Api.AddressAny -> HydraHeadInput (Either Text (Api.UTxO Api.BabbageEra))
  HydraHeadNewTx :: Api.AddressAny -> Text -> HydraHeadInput (Either Text ())
  HydraHeadClose :: Int32 -> Api.AddressAny -> HydraHeadInput (Either Text ())

data Peer = Peer
  { _peer_ip :: String
  , _peer_port :: Word16
  }

data HydraNodeConfig = HydraNodeConfig
  { _hydraNodeConfig_nodeId :: Int32
  , _hydraNodeConfig_port :: Port
  , _hydraNodeConfig_apiPort :: Port
  , _hydraNodeConfig_monitoringPort :: Port
  , _hydraNodeConfig_peers :: [Peer]
  , _hydraNodeConfig_hydraSigningKey :: FilePath
  , _hydraNodeConfig_hydraVerificationKeys :: [FilePath]
  , _hydraNodeConfig_hydraScriptsTxId :: TxId
  , _hydraNodeConfig_cardanoSigningKey :: FilePath
  , _hydraNodeConfig_cardanoVerificationKeys :: [FilePath]
  , _hydraNodeConfig_ledgerGenesis :: FilePath
  , _hydraNodeConfig_ledgerProtocolParams :: FilePath
  , _hydraNodeConfig_magic :: Int32
  , _hydraNodeConfig_cardanoNodeSocket :: FilePath
  , _hydraNodeConfig_persistenceDirectory :: FilePath
  , _hydraNodeConfig_logFile :: FilePath
  , _hydraNodeConfig_logErrFile :: FilePath
  , _hydraNodeConfig_threadLogFile :: FilePath
  , _hydraNodeConfig_for :: Api.AddressAny
  }

data TwoPartyHeadConfig = TwoPartyHeadConfig
  { _twoPartyHeadConfig_firstParty :: ProxyInfo
  , _twoPartyHeadConfig_secondParty :: ProxyInfo
  , _twoPartyHeadConfig_firstPersistDir :: FilePath
  , _twoPartyHeadConfig_secondPersistDir :: FilePath
  , _twoPartyHeadConfig_firstLogOutFile :: FilePath
  , _twoPartyHeadConfig_firstLogErrFile :: FilePath
  , _twoPartyHeadConfig_secondLogFile :: FilePath
  , _twoPartyHeadConfig_secondLogErrFile :: FilePath
  , _twoPartyHeadConfig_firstThreadLogFile :: FilePath
  , _twoPartyHeadConfig_secondThreadLogFile :: FilePath
  }

data CommsThreadConfig = CommsThreadConfig
  { _commsThreadConfig_hydraNodeConfig :: HydraNodeConfig
  , _commsThreadConfig_headStatus :: TVar HydraHeadStatus
  , _commsThreadConfig_nodeStatus :: TVar HydraNodeStatus
  , _commsThreadConfig_inputs :: TBQueue ClientInput
  }

makeLenses ''HydraHeadManager
makeLenses ''RunningHydraHead
makeLenses ''HydraNodeRequest
makeLenses ''HydraNode
makeLenses ''HydraNodeConfig
makeLenses ''TwoPartyHeadConfig
makeLenses ''CommsThreadConfig

-- | Should this comms thread update the Head Status?
commsThreadIsHeadStateReporter :: CommsThreadConfig -> Bool
commsThreadIsHeadStateReporter cfg =
  -- NOTE currently we just make the first node the state reported
  cfg ^. commsThreadConfig_hydraNodeConfig . hydraNodeConfig_nodeId . to (==1)

-- | Transaction ID on preview from the Hydra 0.10.0 release
-- https://github.com/input-output-hk/hydra/releases/tag/0.10.0
previewScriptTxId :: TxId
previewScriptTxId = TxId "d237926e174a2ca386174a5810d30f0ca6db352219dd7eacdc7d5969ae75d58f"

loopbackAddress :: String
loopbackAddress = "127.0.0.1"

mkLocalPeer :: Port -> Peer
mkLocalPeer = Peer loopbackAddress

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

mkTwoPartyHydraNodeConfigs :: (HasNodeInfo a, HasLogger a, MonadIO m) => a -> TxId -> PortRange -> HydraChainConfig -> TwoPartyHeadConfig -> m (Either Text [HydraNodeConfig])
mkTwoPartyHydraNodeConfigs a scriptsTxId prange (HydraChainConfig genesis pparams) (TwoPartyHeadConfig p1 p2 p1dir p2dir p1log p2log p1LogErr p2LogErr p1tlog p2tlog) = runExceptT $ do
  pa <- ExceptT $ allocatePorts a prange 6
  case allocatedPorts pa of
    [firstPort, firstApiPort, firstMonitorPort, secondPort, secondApiPort, secondMonitorPort] -> do
      pure [ HydraNodeConfig
             1
             firstPort
             firstApiPort
             firstMonitorPort
             [mkLocalPeer secondPort]
             (p1 ^. proxyInfo_hydraSigningKey)
             [p2 ^. proxyInfo_hydraVerificationKey]
             scriptsTxId
             (p1 ^. proxyInfo_signingKey)
             [p2 ^. proxyInfo_verificationKey]
             genesis
             pparams
             (ninfo ^. nodeInfo_magic)
             (ninfo ^. nodeInfo_socketPath)
             p1dir
             p1log
             p1LogErr
             p1tlog
             (p1 ^. proxyInfo_address)
           , HydraNodeConfig
             2
             secondPort
             secondApiPort
             secondMonitorPort
             [mkLocalPeer firstPort]
             (p2 ^. proxyInfo_hydraSigningKey)
             [p1 ^. proxyInfo_hydraVerificationKey]
             scriptsTxId
             (p2 ^. proxyInfo_signingKey)
             [p1 ^. proxyInfo_verificationKey]
             genesis
             pparams
             (ninfo ^. nodeInfo_magic)
             (ninfo ^. nodeInfo_socketPath)
             p2dir
             p2log
             p2LogErr
             p2tlog
             (p2 ^. proxyInfo_address)
           ]
    _ -> throwError "Failed to allocate ports"
  where
    ninfo = a ^. nodeInfo

ensureHeadNodesReady :: (MonadIO m, HasLogger a) => a -> RunningHydraHead -> m ()
ensureHeadNodesReady state h = do
  logInfo state "ensureHeadNodesReady" $ "Checking node status before proceeding"
  liftIO $ atomically $ do
    statuses <- for (h ^. hydraHead_handles) $ \headHandle -> do
      readTVar $ headHandle ^. hydraNode_status
    traceM $ "==> STATUS" <> show statuses
    let
      ready = all (== HydraNodeStatus_PeersConnected) statuses

    check ready

initHydraHead :: HydraHeadInput (Either Text ())
initHydraHead = HydraHeadInit

commitHydraHead :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadInput (Either Text ())
commitHydraHead = HydraHeadCommit

closeHydraHead :: Int32 -> Api.AddressAny -> HydraHeadInput (Either Text ())
closeHydraHead = HydraHeadClose

waitForApi :: Port -> IO ()
waitForApi port = do
  result :: Either IOException () <- try $ runClient "127.0.0.1" (fromIntegral port) "/" $ const $ pure ()
  case result of
    Left _ -> do
      threadDelay 100000
      waitForApi port
    Right _ -> pure ()

runHydraHead
  :: (MonadIO m, HasLogger a, HasNodeInfo a, Db.HasDbConnectionPool a)
  => a
  -> Int32
  -> [HydraNodeConfig]
  -> m RunningHydraHead
runHydraHead a headId configs = liftIO $ do
  headStatus <- newTVarIO HydraHead_Uninitialized
  let log = logDebug a "node-supervisor"
  handles <- for configs $ \config -> do
    nodeStatus <- newTVarIO HydraNodeStatus_Unavailable
    process <- newEmptyTMVarIO
    communicationThread <- newEmptyTMVarIO
    inputs <- newTBQueueIO 1000
    outputs <- newBroadcastTChanIO
    let updateTMVar var new = atomically $ do
            isEmpty <- isEmptyTMVar var
            if isEmpty
              then putTMVar var new
              else void $ swapTMVar var new
        runNode = do
          log "Starting hydra node"
          atomically $ writeTVar nodeStatus HydraNodeStatus_Unavailable
          nodeProc <- createProcess <=< mkHydraNodeProc $ config
          _ <- updateTMVar process nodeProc
          comms <- forkIO $ do
            let
              apiPort = config ^. hydraNodeConfig_apiPort
              isReporter = commsThreadIsHeadStateReporter $ CommsThreadConfig config headStatus nodeStatus inputs
              loggerName = bool "commsThread" "reporterThread" isReporter <> tShow (config ^. hydraNodeConfig_nodeId)
            logInfo a loggerName "Waiting for API"
            waitForApi apiPort
            runClient "127.0.0.1" (fromIntegral apiPort) "/" $ \conn -> do
              atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replaying
              logInfo a loggerName "Connected to API"
              sendInputThread <- forkIO $ forever $ do
                atomically $ do
                  status <- readTVar nodeStatus
                  check $ status == HydraNodeStatus_PeersConnected
                cInput <- atomically $ peekTBQueue inputs
                sendDataMessage conn $ WS.Text (encode cInput) Nothing
                _ <- atomically $ readTBQueue inputs
                logInfo a "sendingThread" $ "Message sent: " <> tShow cInput
                threadDelay 250000
              withPingThread conn 60 (pure ()) $ flip finally (killThread sendInputThread) $ forever $ do
                result <- try $ do
                  payload <- WS.receiveData conn
                  status <- atomically $ readTVar nodeStatus
                  case eitherDecode payload of
                    Right serverOutput -> do
                      logInfo a loggerName $ "Received " <> tShow serverOutput
                      case serverOutput of
                        HeadIsInitializing _ _ -> do
                          when isReporter $ when (status == HydraNodeStatus_Replayed) $ Db.runBeam a $ updatePaymentChannelStatusQ headId $ PaymentChannelStatus_Initialized
                        HeadIsOpen _ _ -> do
                          when isReporter $ when (status == HydraNodeStatus_Replayed) $ Db.runBeam a $ updatePaymentChannelStatusQ headId $ PaymentChannelStatus_Open
                        HeadIsClosed _ _ _ -> do
                          when isReporter $ when (status == HydraNodeStatus_Replayed) $ Db.runBeam a $ updatePaymentChannelStatusQ headId $ PaymentChannelStatus_Closed
                        ReadyToFanout _ -> do
                          when isReporter $ when (status == HydraNodeStatus_Replayed) $ Db.runBeam a $ updatePaymentChannelStatusQ headId $ PaymentChannelStatus_CanFanout
                        HeadIsFinalized _ _ -> do
                          when isReporter $ when (status == HydraNodeStatus_Replayed) $ Db.runBeam a $ updatePaymentChannelStatusQ headId $ PaymentChannelStatus_Finalized
                        Greetings _ _ _ -> do
                          atomically $ writeTVar nodeStatus HydraNodeStatus_Replayed
                        PeerConnected _ -> do
                          when (status == HydraNodeStatus_Replayed) $ atomically $ writeTVar nodeStatus HydraNodeStatus_PeersConnected
                        _ -> pure ()

                    Left _ -> do
                      logInfo a loggerName $ "Failed to parse payload: " <> tShow payload

                case result of
                  Left err@(SomeException _) -> do
                    logInfo a loggerName $ "Receive failed: " <> tShow err
                    threadDelay 250000
                  Right _ ->
                    pure ()
          _ <- updateTMVar communicationThread comms
          ec <- waitForProcess $ (\(_, _, _, ph) -> ph) nodeProc
          killThread comms
          pure ec

    nodeRunner <- forkIO $ forever $ withAsync runNode $ \run -> do
      result <- waitCatch run
      case result of
        Left (SomeException e) -> log $ "Hydra node encountered an error: " <> T.pack (show e)
        Right ec -> log $ "Hydra node exited with: " <> T.pack (show ec)
      threadDelay 250000
    pure $ Map.singleton (config ^. hydraNodeConfig_for) $ HydraNode (config ^. hydraNodeConfig_apiPort) process communicationThread nodeStatus nodeRunner inputs outputs
  pure $ RunningHydraHead headStatus (foldOf each handles)

spawnHydraNodeApiConnectionThread
  :: (HasLogger a, HasNodeInfo a, Db.HasDbConnectionPool a, MonadIO m)
  => a -> Int32 -> CommsThreadConfig -> m ThreadId
spawnHydraNodeApiConnectionThread a headId cfg@(CommsThreadConfig config headStatus nodeStatus inputs) = do
  liftIO $ forkIO $ withFile (config ^. hydraNodeConfig_threadLogFile) AppendMode $ \logFile -> forever $ do
    let
      apiPort = config ^. hydraNodeConfig_apiPort
      isReporter = commsThreadIsHeadStateReporter cfg
      loggerName = bool "commsThread" "reporterThread" isReporter
    logInfo a loggerName "Waiting for API to be available"
    waitForApi apiPort
    logInfo a loggerName "API available connecting..."
    runClient "127.0.0.1" (fromIntegral apiPort) "/" $ \conn -> do
      -- We are connected so the nodes will be "replaying"
      atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replaying
      requestThreadId <- forkIO $ forever $ do
        cInput <- atomically $ peekTBQueue inputs
        logInfo a "sendingThread" $ "Sending input: " <> tShow cInput
        LBS.hPutStr logFile $ encode cInput <> "\n"
        hFlush logFile
        -- TODO(skylar): Check for exception here
        sendDataMessage conn $ WS.Text (encode cInput) Nothing
        _ <- atomically $ readTBQueue inputs
        logInfo a "sendingThread" $ "Message sent: " <> tShow cInput
        pure ()
      flip finally (killThread requestThreadId) $ withPingThread conn 60 (pure ()) $ forever $ do
        result <- try $ do
          payload <- WS.receiveData conn
          logInfo a loggerName "Got new message, logging"
          LBS.hPutStr logFile $ payload <> "\n"
          hFlush logFile
          case eitherDecode payload of
            Right res -> do
              status <- atomically $ readTVar nodeStatus
              logInfo a loggerName $ "Node is " <> tShow status
              logInfo a loggerName $ "Valid message, processing...\n" <> tShow res
              case status of
                HydraNodeStatus_Replaying -> handleHydraNodeApiResponseOther loggerName isReporter res
                _ -> handleHydraNodeApiResponse loggerName isReporter res
            Left err -> do
              logInfo a loggerName $ "Invalid message received: " <> tShow payload <> " " <> T.pack err
        case result of
          Left err@(SomeException _) -> do
            logInfo a loggerName $ "Message Handler failed: " <> tShow err
            threadDelay 250000
          Right _ ->
            pure ()
  where
    handleHydraNodeApiResponseOther loggerName isReporter = \case
      Greetings {} -> do
        logInfo a loggerName "Hydra Node Replay Complete"
        atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replayed
      _ -> pure ()
    -- Update hydra-pay state based on hydra node responses
    handleHydraNodeApiResponse loggerName isReporter = \case
      PeerConnected _ -> do
        logInfo a loggerName "Hydra Node ready"
        atomically $ writeTVar nodeStatus $ HydraNodeStatus_PeersConnected
      PostTxOnChainFailed _ _ -> do
        logInfo a loggerName "Hydra Node failed to submit transaction on chain (check for fuel)"
      HeadIsInitializing _ _ -> do
        when (commsThreadIsHeadStateReporter cfg) $ do
          logInfo a loggerName "Head is initializing (waiting for commits)"
          atomically $ writeTVar headStatus $ HydraHead_Initializing
      HeadIsOpen _ _ -> do
        when (commsThreadIsHeadStateReporter cfg) $ do
          logInfo a loggerName "Head is open"
          atomically $ writeTVar headStatus $ HydraHead_Open
          Db.runBeam a $ updatePaymentChannelStatusQ headId PaymentChannelStatus_Open
      HeadIsClosed hid _ deadline -> do
        when isReporter $ do
          logInfo a loggerName $ "Head is closed: " <> tShow hid <> "\ntimeout: " <> tShow deadline
          atomically $ writeTVar headStatus $ HydraHead_Closed
          atomically $ writeTVar nodeStatus $ HydraNodeStatus_Closed
      ReadyToFanout hid -> do
        when isReporter $ do
          logInfo a loggerName $ "Ready to Fanout:" <> tShow hid
          traceM $ "Ready to Fanout: STATUS PRE"
          status <- atomically $ readTVar nodeStatus
          traceM $ "Ready to Fanout: STATUS" <> show status
          -- when (status /= HydraNodeStatus_Replaying) $ do
            -- liftIO $ atomically $ writeTBQueue pendingCommands Fanout
      HeadIsFinalized _ utxoJson -> when isReporter $ do
        case Aeson.fromJSON utxoJson of
          Aeson.Error e ->
            print e
          Aeson.Success (Api.UTxO utxoMap :: Api.UTxO Api.BabbageEra) -> do
            let addressAndValues = txOutAddressAndValue <$> Map.elems utxoMap
                -- Combine UTxOs going to the same address
                addressAndValuesTotal = Map.fromListWith (<>) addressAndValues
            iforM_ addressAndValuesTotal $ \proxyAddr val -> Db.runBeam a $ do
              mChainAddress <- getProxyChainAddressAndSigningKey proxyAddr
              forM_ mChainAddress $ \(chainAddress, skPath) -> do
                let lovelace = Api.selectLovelace val
                    toL1Adddress = chainAddress
                Right pparams <- runCardanoCli a getProtocolParameters
                liftIO $ fanoutToL1Address a pparams proxyAddr (T.unpack skPath) toL1Adddress $ fromIntegral lovelace
              pure ()
      _ -> pure ()

txOutAddressAndValue :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> (Api.AddressAny, Api.Value)
txOutAddressAndValue (Api.TxOut (Api.AddressInEra _ a) val _ _) = (Api.toAddressAny a, Api.txOutValueToValue val)

unsafeAnyNode :: RunningHydraHead -> HydraNode
unsafeAnyNode = head . Map.elems . _hydraHead_handles

getNodeFor :: RunningHydraHead -> Api.AddressAny -> Maybe HydraNode
getNodeFor hHead addr = Map.lookup addr $ hHead ^. hydraHead_handles

sendClientInput :: (MonadIO m) => RunningHydraHead -> Maybe Api.AddressAny -> ClientInput -> m (Either Text ())
sendClientInput runningHead mAddress input = liftIO $ do
  case mNode of
    Just node -> do
      atomically $ writeTBQueue (node ^. hydraNode_inputs) input
      pure $ Right ()
    Nothing -> pure $ Left "Falied to send client input, unable to locate suitable node"
  where
    mNode = case mAddress of
      Just addr -> getNodeFor runningHead addr
      Nothing -> pure $ unsafeAnyNode runningHead


-- sendHydraHeadCommand :: (MonadIO m, HasLogger a) => a -> RunningHydraHead -> HydraHeadInput b -> m b
-- sendHydraHeadCommand a hHead command = do
--   ensureHeadNodesReady a hHead
--   liftIO $ threadDelay 5000000
--   logInfo a "sendHydraHeadCommand" "Nodes are ready, sending command"
--   case command of
--     HydraHeadInit -> do
--       let
--         -- We don't care for init!
--         node = unsafeAnyNode hHead
--       output <- performHydraNodeRequest node Init
--       case output of
--         CommandFailed cm -> pure $ Left $ "Command Failed: " <> T.pack (show cm)
--         HeadIsInitializing _ _ -> pure $ Right ()
--         _ -> pure $ Left $ "Invalid response received" <> tShow output
--
--     HydraHeadCommit addr utxo' -> runExceptT $ do
--       node <- case getNodeFor hHead addr of
--         Nothing -> throwError $ "Address " <> Api.serialiseAddress addr <> " is not a part of this head!"
--         Just node ->  pure node
--       output <- performHydraNodeRequest node $ Commit $ Aeson.toJSON utxo'
--       case output of
--         CommandFailed cm -> throwError $ "Commit Failed: " <> T.pack (show cm)
--         Committed _ _ _ -> pure ()
--         _ -> throwError $ "Invalid response received" <> tShow output
--
--     HydraHeadGetAddressUTxO addr -> runExceptT $ do
--       let node = unsafeAnyNode hHead
--       output <- performHydraNodeRequest node GetUTxO
--       traceM $ show output
--       case output of
--         GetUTxOResponse _ utxoJson -> do
--           case Aeson.fromJSON utxoJson of
--             Aeson.Success utxo' -> pure $ filterUTxOByAddress addr utxo'
--             Aeson.Error e -> throwError $ "Failed to decode GetUTxOResponse: " <> T.pack e
--         CommandFailed _ ->
--           throwError "GetUTxO Failed"
--         _ ->
--           throwError $ "Invalid response received" <> tShow output
--
--     HydraHeadNewTx addr txCBOR -> runExceptT $ do
--       node <- case getNodeFor hHead addr of
--         Nothing -> throwError $ "Address " <> Api.serialiseAddress addr <> " is not a part of this head!"
--         Just node ->  pure node
--       output <- performHydraNodeRequest node $ NewTx $ Aeson.String txCBOR
--       case output of
--         TxValid {} -> do
--           traceM $ "NewTx: " <> show output
--           pure ()
--         SnapshotConfirmed _ snapshotJson _ -> do
--           traceM $ "NewTx: SnapshotConfirmed: " <> show snapshotJson
--         TxInvalid _ _ _ _ -> throwError "NewTx Failed"
--         _ -> do
--           traceM $ "Invalid response received" <> show output
--           throwError $ "Invalid response received" <> tShow output
--
--     HydraHeadClose _hid _ -> do
--       let node = unsafeAnyNode hHead
--       output <- performHydraNodeRequest node Close
--       case output of
--         CommandFailed _ -> pure $ Left "Command Failed"
--         HeadIsClosed _ _ _ -> pure $ Right ()
--         _ -> pure $ Left $ "Invalid response received" <> tShow output
--
filterUTxOByAddress :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> Api.UTxO Api.BabbageEra
filterUTxOByAddress addr (Api.UTxO m) = Api.UTxO $ Map.filter (\x -> txOutAddress x == addr) m
  where
    txOutAddress (Api.TxOut (Api.AddressInEra _ a) _ _ _) = Api.toAddressAny a

logTo :: Handle -> Handle -> CreateProcess -> CreateProcess
logTo out err cp =
  cp { std_out = UseHandle out
     , std_err = UseHandle err
     }

mkHydraNodeProc :: MonadIO m => HydraNodeConfig -> m CreateProcess
mkHydraNodeProc cfg = do
  out <- liftIO $ openFile (cfg ^. hydraNodeConfig_logFile) AppendMode
  err <- liftIO $ openFile (cfg ^. hydraNodeConfig_logErrFile) AppendMode
  pure $ logTo out err $ proc hydraNodePath (hydraNodeArgs cfg)

hydraNodeArgs :: HydraNodeConfig -> [String]
hydraNodeArgs cfg = join
   [ [ "--node-id"
     , cfg ^. hydraNodeConfig_nodeId . to show
     , "--port"
     , cfg ^. hydraNodeConfig_port . to show
     , "--api-port"
     , cfg ^. hydraNodeConfig_apiPort . to show
     , "--monitoring-port"
     , cfg ^. hydraNodeConfig_monitoringPort . to show
     ]
   , cfg ^. hydraNodeConfig_peers ^.. traversed . to peerArg . folded
   , [ "--hydra-signing-key"
     , cfg ^. hydraNodeConfig_hydraSigningKey
     ]
   , cfg ^. hydraNodeConfig_hydraVerificationKeys ^.. traversed . to hydraVerificationKeyArg . folded
   , [ "--hydra-scripts-tx-id"
     , cfg ^. hydraNodeConfig_hydraScriptsTxId . to (T.unpack . unTxId)
     , "--cardano-signing-key"
     , cfg ^. hydraNodeConfig_cardanoSigningKey
     ]
   , cfg ^. hydraNodeConfig_cardanoVerificationKeys ^.. traversed . to cardanoVerificationKeyArg . folded
   , [ "--ledger-genesis"
     , cfg ^. hydraNodeConfig_ledgerGenesis
     , "--ledger-protocol-parameters"
     , cfg ^. hydraNodeConfig_ledgerProtocolParams
     , "--testnet-magic"
     , cfg ^. hydraNodeConfig_magic . to show
     , "--node-socket"
     , cfg ^. hydraNodeConfig_cardanoNodeSocket
     , "--persistence-dir"
     , cfg ^. hydraNodeConfig_persistenceDirectory
     ]
   ]
  where
    hydraVerificationKeyArg :: FilePath -> [String]
    hydraVerificationKeyArg vkPath =
      [ "--hydra-verification-key"
      , vkPath
      ]

    cardanoVerificationKeyArg :: FilePath -> [String]
    cardanoVerificationKeyArg vkPath =
      [ "--cardano-verification-key"
      , vkPath
      ]

    peerArg :: Peer -> [String]
    peerArg (Peer ip port) =
      [ "--peer"
      , ip <> ":" <> show port
      ]

deriveConfigFromDbHead :: (MonadIO m, MonadBeamInsertReturning Postgres m, HasPortRange a, HasLogger a, HasNodeInfo a) => a -> Db.HydraHead -> m (Either Text [HydraNodeConfig])
deriveConfigFromDbHead a hh = runExceptT $ do
  liftIO $ do
    createDirectoryIfMissing True headPersistDir
    createDirectoryIfMissing True headNodeLogsDir

  let
    firstText = hh ^. Db.hydraHead_first
    secondText = hh ^. Db.hydraHead_second
    chain = HydraChainConfig (T.unpack $ hh ^. Db.hydraHead_ledgerGenesis) (T.unpack $ hh ^. Db.hydraHead_ledgerProtocolParams)

  first <- ExceptT $ pure $ maybeToEither "Failed to deserialize address" $ Api.deserialiseAddress Api.AsAddressAny firstText
  second <- ExceptT $ pure $ maybeToEither "Failed to deserialize address" $ Api.deserialiseAddress Api.AsAddressAny secondText

  firstProxy <- ExceptT $ queryProxyInfo a (Db.hydraHeadId hh) first
  secondProxy <- ExceptT $ queryProxyInfo a (Db.hydraHeadId hh) second

  let
    suffixFirst = (T.unpack $ (tShow $ Db.hydraHeadId hh) <> "-" <> T.takeEnd 8 firstText)
    suffixSecond = (T.unpack $ (tShow $ Db.hydraHeadId hh) <> "-" <> T.takeEnd 8 secondText)
    persistFirst = headPersistDir </> suffixFirst
    persistSecond = headPersistDir </> suffixSecond
    logFirst = headNodeLogsDir </> (suffixFirst <> ".log")
    logSecond = headNodeLogsDir </> (suffixSecond <> ".log")
    logErrFirst = headNodeLogsDir </> (suffixFirst <> ".error.log")
    logErrSecond = headNodeLogsDir </> (suffixSecond <> ".error.log")
    tlogFirst = headNodeLogsDir </> (suffixFirst <> ".thread.log")
    tlogSecond = headNodeLogsDir </> (suffixSecond <> ".thread.log")

    headConfig = TwoPartyHeadConfig
      firstProxy
      secondProxy
      persistFirst
      persistSecond
      logFirst
      logSecond
      logErrFirst
      logErrSecond
      tlogFirst
      tlogSecond

  liftIO $ do
    createDirectoryIfMissing True persistFirst
    createDirectoryIfMissing True persistSecond

  nodeConfigs <- ExceptT $ mkTwoPartyHydraNodeConfigs a previewScriptTxId (a ^. portRange) chain headConfig
  pure nodeConfigs

headPersistDir :: FilePath
headPersistDir = "hydra-head-persistence"

headNodeLogsDir :: FilePath
headNodeLogsDir = "hydra-node-logs"

getRunningHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> m (Either Text (TMVar RunningHydraHead))
getRunningHead a hid = do
  logInfo a "getRunningHead" $ "Fetching head " <> tShow hid
  liftIO $ withTMVar runningHeads $ \running -> do
    traceM $ "RUNNING => " <> show (Map.keys running)
    case Map.lookup hid running of
      Just h -> pure (running, Right h)
      Nothing -> pure (running, Left $ "Running head with id " <> tShow hid <> " couldn't be found")
  where
    runningHeads = a ^. hydraHeadManager . hydraHeadManager_runningHeads

trackRunningHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> RunningHydraHead -> m (Either Text ())
trackRunningHead a hid hydraHead = do
  logInfo a "trackRunningHead" $ "Adding a running head to the hydra head manager"
  liftIO $ withTMVar runningHeads $ \running -> do
    case Map.lookup hid running of
      Just _ -> do
        let errMsg = "Running head already for " <> tShow hid
        logWarn a "trackRunningHead" errMsg
        pure (running, Left errMsg)
      Nothing -> do
        logInfo a "trackRunningHead" $ "New head added to head manager: " <> tShow hid
        headVar <- newTMVarIO hydraHead
        pure (Map.insert hid headVar running, Right ())
  where
    runningHeads = a ^. hydraHeadManager . hydraHeadManager_runningHeads

withHydraHeadManager :: (HydraHeadManager -> IO a) -> IO a
withHydraHeadManager action = do
  bracket newHydraHeadManager terminateRunningHeads action

startupExistingHeads :: (MonadIO m, MonadBeamInsertReturning Postgres m, Db.HasDbConnectionPool a, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a) => a -> m (Either Text Int)
startupExistingHeads a = do
  activeHeads <- activeHydraHeads
  results <- for activeHeads $ \dbHead -> runExceptT $ do
    nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
    let headId = Db.hydraHeadId dbHead
    hydraHead <- lift $ runHydraHead a headId nodeConfigs
    ExceptT $ trackRunningHead a headId hydraHead
  pure $ fmap (sum . fmap (const 1)) $ sequenceA results

activeHydraHeads :: MonadBeam Postgres m => m [Db.HydraHead]
activeHydraHeads = do
  runSelectReturningList $ select $ do
    heads_ <- all_ (Db.db ^. Db.db_heads)
    paymentChan_ <- join_ (Db.db ^. Db.db_paymentChannels) (\paymentChan -> (paymentChan ^. Db.paymentChannel_head) `references_` heads_)
    guard_ (paymentChan_ ^. Db.paymentChannel_status /=. val_ PaymentChannelStatus_Closed)
    pure $ heads_

spinUpHead :: (MonadIO m, MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, Db.HasDbConnectionPool a, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a) => a -> Int32 -> m (Either Text ())
spinUpHead a hid = runExceptT $ do
  mHead <- do
    runSelectReturningOne $ select $ do
      h <- all_ (Db.db ^. Db.db_heads)
      guard_ (h ^. Db.hydraHead_id ==. val_ (SqlSerial hid))
      pure h
  case mHead of
    Nothing -> throwError $ "Invalid head id" <> tShow hid
    Just dbHead -> do
      traceM $ "spinUpHead: Found head: " <>  show (Db._hydraHead_id dbHead)
      nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
      traceM $ "spinUpHead: Configs retrieved"
      hydraHead <- lift $ runHydraHead a hid nodeConfigs
      traceM $ "spinUpHead: Ran Hydra Head"
      out <- ExceptT $ trackRunningHead a hid hydraHead
      traceM $ "spinUpHead: Tracked Running Head"
      pure out

newHydraHeadManager :: MonadIO m => m HydraHeadManager
newHydraHeadManager = do
  runningHeads <- liftIO $ newTMVarIO mempty
  pure $ HydraHeadManager
    { _hydraHeadManager_runningHeads = runningHeads
    }

terminateRunningHeads :: MonadIO m => HydraHeadManager -> m ()
terminateRunningHeads (HydraHeadManager channels) = do
  withTMVar channels $ \running -> do
    for_ (Map.elems running) $ \headVar -> do
      withTMVar headVar $ \hydraHead@(RunningHydraHead _ handles) -> do
        for_ handles $ \(HydraNode _ nodeHandleRef thread _ runner _ _) -> liftIO $ do
          killThread runner
          mNodeHandle <- atomically $ tryReadTMVar nodeHandleRef
          forM_ mNodeHandle $ \nodeHandle@(_, out, err, _) -> do
            cleanupProcess nodeHandle
            maybe (pure ()) hClose out
            maybe (pure ()) hClose err
          killThread <=< atomically $ readTMVar thread
        pure (hydraHead, ())
    pure (running, ())

{-
initHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> m (Either Text ())
initHead a hid = do
  logInfo a "initHead" $ "Attempting to Init head " <> tShow hid
  result <- runExceptT $ do
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand a hydraHead $ initHydraHead
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "initHead" $ "Head " <> tShow hid <> " is initialized"
    Left err -> logInfo a "initHead" $ "Head failed to initialize: " <> err
  pure result

commitToHead :: (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m,  MonadIO m, HasNodeInfo a, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> Api.Lovelace -> m (Either Text ())
commitToHead a hid committer amount = do
  result <- runExceptT $ do
    proxyAddr <- fmap _proxyInfo_address $ ExceptT $ queryProxyInfo a hid committer
    logInfo a "commitHead" $ "Attempting to Commit from address: " <> Api.serialiseAddress proxyAddr
    balance <- ExceptT $ runCardanoCli a $ queryUTxOs proxyAddr
    commitUtxo <- ExceptT $ pure $ maybeToEither ("Failed to find suitable commit utxo with " <> tShow amount <> "lovelace") $ findUTxOWithExactly amount balance
    logInfo a "commitHead" $ "Attempting to Commit to " <> tShow hid
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand a hydraHead $ commitHydraHead proxyAddr commitUtxo
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "commitHead" $ "Head " <> tShow hid <> " was committed to by " <> Api.serialiseAddress committer
    Left err -> logInfo a "commitHead" $ "Head failed to commit from " <> Api.serialiseAddress committer <> ": " <> err
  pure result
-}
{-
getAddressUTxO :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> m (Either Text (Api.UTxO Api.BabbageEra))
getAddressUTxO a hid committer = do
  runExceptT $ do
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand a hydraHead $ HydraHeadGetAddressUTxO committer
      pure (hydraHead, result)
-}
{-
newTx :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> Text -> Text -> m (Either Text ())
newTx a hid addr _txid txCBOR = do
  runExceptT $ do
    traceM "newTx: Start"
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      traceM "newTx: Pre cmd"
      result <- sendHydraHeadCommand a hydraHead $ HydraHeadNewTx addr txCBOR
      traceM "newTx: Post cmd"
      pure (hydraHead, result)
-}
{-
closeHead :: (MonadBeam Postgres m, MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> m (Either Text ())
closeHead a hid committer = do
  result <- runExceptT $ do
    logInfo a "closeHead" $ "Attempting to Close " <> tShow hid
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand a hydraHead $ closeHydraHead hid committer
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "closeHead" $ "Head " <> tShow hid <> " was closed by " <> Api.serialiseAddress committer
    Left err -> logInfo a "closeHead" $ "Head failed to commit from " <> Api.serialiseAddress committer <> ": " <> err
  pure result
-}
