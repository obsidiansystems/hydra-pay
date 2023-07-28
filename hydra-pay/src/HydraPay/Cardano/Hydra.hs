{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra where

import Prelude hiding (log)

import System.IO
import System.Which
import System.Process
import System.FilePath
import System.Directory

import Data.Int
import Data.Word
import Data.Text (Text)
import Data.Bool (bool)
import Data.Foldable
import Data.Traversable
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Lens
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Proxy
import HydraPay.Logging
import HydraPay.Cardano.Hydra.Status
import HydraPay.Cardano.Hydra.ChainConfig (HydraChainConfig(..))
import HydraPay.Cardano.Node
import HydraPay.Database.Workers
import HydraPay.PaymentChannel.Postgres
import HydraPay.PaymentChannel (PaymentChannelStatus(..))
import HydraPay.PortRange
import HydraPay.Cardano.Hydra.RunningHead
import HydraPay.Cardano.Hydra.Api hiding (headId, headStatus, Party)
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

import Cardano.Api.Extras


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
  , _hydraNodeConfig_for :: ProxyAddress
  }

data Party =
  ProxyParty ProxyInfo | NonProxyParty Api.AddressAny ProxyInfo

getPartyProxyInfo :: Party -> ProxyInfo
getPartyProxyInfo (ProxyParty p) = p
getPartyProxyInfo (NonProxyParty _ p) = p

getPartyAddress :: Party -> ProxyAddress
getPartyAddress (ProxyParty p) = p ^. proxyInfo_address
getPartyAddress (NonProxyParty a _) = ProxyAddress a

data TwoPartyHeadConfig = TwoPartyHeadConfig
  { _twoPartyHeadConfig_firstParty :: Party
  , _twoPartyHeadConfig_secondParty :: Party
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

-- | Transaction ID on preview from the Hydra 0.11.0 release
-- https://github.com/input-output-hk/hydra/releases/tag/0.11.0
previewScriptTxId :: TxId
previewScriptTxId = TxId "90acbeb0ebece3b5319625eedca3f6514870c9414872d9e940c6b7d7b88178fd"

loopbackAddress :: String
loopbackAddress = "127.0.0.1"

mkLocalPeer :: Port -> Peer
mkLocalPeer = Peer loopbackAddress

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

mkTwoPartyHydraNodeConfigs :: (HasNodeInfo a, HasLogger a, MonadIO m) => a -> TxId -> PortRange -> HydraChainConfig -> TwoPartyHeadConfig -> m (Either Text [HydraNodeConfig])
mkTwoPartyHydraNodeConfigs a scriptsTxId prange (HydraChainConfig genesis pparams) (TwoPartyHeadConfig party1 party2 p1dir p2dir p1log p2log p1LogErr p2LogErr p1tlog p2tlog) = runExceptT $ do
  let
    p1 = getPartyProxyInfo party1
    p2 = getPartyProxyInfo party2
    p1For = getPartyAddress party1
    p2For = getPartyAddress party2
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
             (p1 ^. proxyInfo_internalWalletSigningKey)
             [p2 ^. proxyInfo_internalWalletVerificationKey]
             genesis
             pparams
             (ninfo ^. nodeInfo_magic)
             (ninfo ^. nodeInfo_socketPath)
             p1dir
             p1log
             p1LogErr
             p1tlog
             p1For
           , HydraNodeConfig
             2
             secondPort
             secondApiPort
             secondMonitorPort
             [mkLocalPeer firstPort]
             (p2 ^. proxyInfo_hydraSigningKey)
             [p1 ^. proxyInfo_hydraVerificationKey]
             scriptsTxId
             (p2 ^. proxyInfo_internalWalletSigningKey)
             [p1 ^. proxyInfo_internalWalletVerificationKey]
             genesis
             pparams
             (ninfo ^. nodeInfo_magic)
             (ninfo ^. nodeInfo_socketPath)
             p2dir
             p2log
             p2LogErr
             p2tlog
             p2For
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
  :: (MonadIO m, HasLogger a, Db.HasDbConnectionPool a)
  => a
  -> Int32
  -> [HydraNodeConfig]
  -> m RunningHydraHead
runHydraHead a headId configs = liftIO $ do
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
              isReporter = commsThreadIsHeadStateReporter $ CommsThreadConfig config nodeStatus inputs
              loggerName = "Head " <> tShow headId <> "-" <> bool "commsThread" "reporterThread" isReporter <> "-node" <> tShow (config ^. hydraNodeConfig_nodeId)
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
                      logInfo a loggerName $ "Received(" <> tShow status <> ") " <> tShow serverOutput
                      case serverOutput of
                        HeadIsInitializing _ _ -> do
                          when isReporter $ Db.runBeam a $ do
                            updateHydraHeadStatusQ headId $ HydraHeadStatus_Initialized
                            tryUpdatePaymentChannelForHeadStatusQ headId $ PaymentChannelStatus_WaitingForAccept
                        HeadIsOpen _ _ -> do
                          when isReporter $ Db.runBeam a $ do
                            updateHydraHeadStatusQ headId $ HydraHeadStatus_Open
                            tryUpdatePaymentChannelForHeadStatusQ headId $ PaymentChannelStatus_Open
                        HeadIsClosed _ _ _ -> do
                          when isReporter $ Db.runBeam a $ updateHydraHeadStatusQ headId $ HydraHeadStatus_Closed
                        ReadyToFanout _ -> do
                          when isReporter $ do
                            atomically $ writeTBQueue inputs Fanout
                        HeadIsFinalized _ _ -> do
                          when isReporter $ Db.runBeam a $ do
                            updateHydraHeadStatusQ headId $ HydraHeadStatus_Finalized
                            addPaymentChannelTask' $ PaymentChannelReq_Cleanup headId
                        Greetings _ _ _ -> do
                          atomically $ writeTVar nodeStatus HydraNodeStatus_Replayed
                        PeerConnected _ -> do
                          atomically $ writeTVar nodeStatus HydraNodeStatus_PeersConnected
                        Committed _ party_ _ -> do
                          let
                            mVkey = party_ ^? key "vkey" . _String
                          case mVkey of
                            Just vkey -> do
                              when isReporter $ Db.runBeam a $ do
                                mCommit <- runSelectReturningOne $ select $ do
                                  commit <- all_ (Db.db ^. Db.db_observedCommits)
                                  guard_ ((commit ^. Db.observedCommit_vkey ==. val_ vkey) &&. (commit ^. Db.observedCommit_head ==. (val_ $ Db.HeadId $ SqlSerial headId)))
                                  pure commit
                                case mCommit of
                                  Just _ -> logInfo a loggerName $ "Commit already observed"
                                  Nothing -> do
                                    logInfo a loggerName $ "Recording observed commit"
                                    _ <- runInsert $ insert (Db.db ^. Db.db_observedCommits) $
                                      (insertExpressions [ Db.ObservedCommit
                                        default_
                                        (val_ vkey)
                                        current_timestamp_
                                        (val_ $ Db.HeadId $ SqlSerial headId)
                                      ])
                                    -- joinPaymentChannel headId
                                    pure ()

                            Nothing -> do
                              logInfo a loggerName $ "Failed to parse vkey from Committed payload: " <> tShow party_
                              pure ()

                          pure ()
                        _ -> pure ()

                      when (status == HydraNodeStatus_Replayed || status == HydraNodeStatus_PeersConnected) $ do
                        atomically $ writeTChan outputs $ serverOutput

                    Left _ -> do

                      logInfo a loggerName $ "Failed to parse payload: " <> tShow payload

                case result of
                  Left err@(SomeException _) -> do
                    logInfo a loggerName $ "Receive failed: " <> tShow err
                    status <- Db.runBeam a $ getHydraHeadStatusQ headId

                    when (status == HydraHeadStatus_Done) $ killThread <=< atomically $ readTMVar communicationThread
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
    pure $ Map.singleton (config ^. hydraNodeConfig_for) $ HydraNode (config ^. hydraNodeConfig_port) (config ^. hydraNodeConfig_apiPort) process communicationThread nodeStatus nodeRunner inputs outputs
  pure $ RunningHydraHead $ foldOf each handles

txOutAddressAndValue :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> (Api.AddressAny, Api.Value)
txOutAddressAndValue (Api.TxOut (Api.AddressInEra _ a) val _ _) = (Api.toAddressAny a, Api.txOutValueToValue val)

unsafeAnyNode :: RunningHydraHead -> HydraNode
unsafeAnyNode = head . Map.elems . _hydraHead_handles

getNodeFor :: RunningHydraHead -> ProxyAddress -> Maybe HydraNode
getNodeFor hHead addr = Map.lookup addr $ hHead ^. hydraHead_handles

getNodeFor' :: RunningHydraHead -> ProxyAddress -> Either Text HydraNode
getNodeFor' hHead addr =
  case Map.lookup addr $ hHead ^. hydraHead_handles of
    Nothing -> Left $ "Failed to get node for " <> Api.serialiseAddress addr
    Just node -> Right node

sendClientInput :: (MonadIO m) => RunningHydraHead -> Maybe ProxyAddress -> ClientInput -> m (Either Text ())
sendClientInput runningHead mAddress input_ = liftIO $ do
  case mNode of
    Just node -> do
      atomically $ writeTBQueue (node ^. hydraNode_inputs) input_
      pure $ Right ()
    Nothing -> pure $ Left "Falied to send client input, unable to locate suitable node"
  where
    mNode = case mAddress of
      Just addr -> getNodeFor runningHead addr
      Nothing -> pure $ unsafeAnyNode runningHead

waitForServerOutput :: MonadIO m => TChan ServerOutput -> (ServerOutput -> Bool) -> m ServerOutput
waitForServerOutput outputs filterFunc = liftIO $ do
  atomically $ do
    o <- readTChan outputs
    check $ filterFunc o
    pure o

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
   , [ -- "--ledger-genesis"
     -- , cfg ^. hydraNodeConfig_ledgerGenesis
     "--ledger-protocol-parameters"
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
    (firstParty, secondParty) =
      case hh ^. Db.hydraHead_usesProxies of
        True -> (ProxyParty firstProxy, ProxyParty secondProxy)
        False -> (NonProxyParty (unsafeToAddressAny $ hh ^. Db.hydraHead_first) firstProxy, NonProxyParty (unsafeToAddressAny $ hh ^. Db.hydraHead_second) secondProxy)

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
      firstParty
      secondParty
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

untrackRunningHead :: (MonadIO m, HasHydraHeadManager a) => a -> Int32 -> m ()
untrackRunningHead a hid = do
  withTMVar runningHeads $ \running -> do
    case Map.lookup hid running of
      Just rh -> terminateRunningHead rh
      _ -> pure ()
    pure (Map.delete hid running, ())
  where
    runningHeads = a ^. hydraHeadManager . hydraHeadManager_runningHeads

withHydraHeadManager :: (HydraHeadManager -> IO a) -> IO a
withHydraHeadManager action = do
  bracket newHydraHeadManager terminateRunningHeads action

startupExistingHeads :: (MonadIO m, MonadBeamInsertReturning Postgres m, Db.HasDbConnectionPool a, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a) => a -> m (Either Text Int)
startupExistingHeads a = do
  ensureNodeSocket a
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
    head_ <- all_ (Db.db ^. Db.db_heads)
    guard_ (head_ ^. Db.hydraHead_status /=. val_ HydraHeadStatus_Done)
    pure $ head_

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
      nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
      hydraHead <- lift $ runHydraHead a hid nodeConfigs
      out <- ExceptT $ trackRunningHead a hid hydraHead
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
    for_ (Map.elems running) terminateRunningHead
    pure (running, ())

terminateRunningHead :: MonadIO m => TMVar RunningHydraHead -> m ()
terminateRunningHead hVar =
  withTMVar_ hVar $ \(RunningHydraHead handles) -> liftIO $ do
    for_ handles $ \(HydraNode _ _ nodeHandleRef thread _ runner _ _) -> do
      killThread runner
      mNodeHandle <- atomically $ tryReadTMVar nodeHandleRef
      forM_ mNodeHandle $ \nodeHandle@(_, out, err, _) -> do
        cleanupProcess nodeHandle
        maybe (pure ()) hClose out
        maybe (pure ()) hClose err
      killThread <=< atomically $ readTMVar thread

getAddressUTxO :: (MonadIO m, HasLogger a, HasHydraHeadManager a, ToAddress b) => a -> Int32 -> b -> m (Either Text (Api.UTxO Api.BabbageEra))
getAddressUTxO a hid b = runExceptT $ do
  runningHeadVar <- ExceptT $ getRunningHead a hid
  chan <- ExceptT $ withTMVar runningHeadVar $ \rh -> do

    result <- runExceptT $ do
      node <- ExceptT $ pure $ getNodeFor' rh $ ProxyAddress addr
      output <- liftIO $ atomically $ dupTChan $ node ^. hydraNode_outputs
      ExceptT $ sendClientInput rh (Just $ ProxyAddress addr) $ GetUTxO
      pure output
    pure (rh, result)

  ExceptT $ liftIO $ atomically $ do
    o <- readTChan chan
    case o of
      GetUTxOResponse _ utxo -> pure $ Right $ filterUTxOByAddress addr utxo
      CommandFailed GetUTxO -> pure $ Left "GetUTxO: Command Failed"
      x -> pure $ Left $ "Got something else: " <> tShow x
  where
    addr = toAddress b

addPaymentChannelTask' :: MonadBeam Postgres m => PaymentChannelReq -> m ()
addPaymentChannelTask' payload = runInsert $ insert (Db._db_paymentChanTask Db.db) $
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
