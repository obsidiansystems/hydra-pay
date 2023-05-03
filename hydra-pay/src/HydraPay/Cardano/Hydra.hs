{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra where

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
import HydraPay.Cardano.Node
import HydraPay.PortRange
import HydraPay.Cardano.Hydra.Api
import qualified HydraPay.Database as Db

import qualified Cardano.Api as Api

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions

import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LBS

import Network.WebSockets

data HydraHeadManager = HydraHeadManager
  { _hydraHeadManager_runningHeads :: TMVar (Map Int32 (TMVar RunningHydraHead))
  }

class HasHydraHeadManager a where
  hydraHeadManager :: Lens' a HydraHeadManager

instance HasHydraHeadManager HydraHeadManager where
  hydraHeadManager = id

data HydraNodeStatus
  = HydraNodeStatus_Unavailable
  | HydraNodeStatus_Replaying
  | HydraNodeStatus_Replayed
  | HydraNodeStatus_PeersConnected
  deriving (Eq, Show)

data HydraNodeRequest = HydraNodeRequest
  { _hydraNodeRequest_id :: Int
  , _hydraNodeRequest_clientInput :: ClientInput
  , _hydraNodeRequest_mailbox :: TMVar ServerOutput
  }

data HydraNode = HydraNode
  { _hydraNode_apiPort :: Port
  , _hydraNode_processInfo :: ProcessInfo
  , _hydraNode_communicationThread :: ThreadId
  , _hydraNode_status :: TVar HydraNodeStatus
  , _hydraNode_pendingRequests :: TMVar (Map Int HydraNodeRequest)
  , _hydraNode_requestQueue :: TBQueue ClientInput
  }

data HydraHeadInput :: * -> * where
  HydraHeadInit :: HydraHeadInput (Either Text ())
  HydraHeadCommit :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadInput (Either Text ())

type ProcessInfo = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data HydraHeadStatus
  = HydraHead_Uninitialized
  | HydraHead_Initializing
  | HydraHead_Open
  | HydraHead_Closed
  | HydraHead_Finalized
  | HydraHead_Aborted

-- | A running Hydra Head
data RunningHydraHead = RunningHydraHead
  { _hydraHead_status :: TVar HydraHeadStatus
  , _hydraHead_handles :: Map Api.AddressAny HydraNode
  }

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

data HydraChainConfig = HydraChainConfig
  { _hydraChainConfig_ledgerGenesis :: FilePath
  , _hydraChainConfig_ledgerProtocolParams :: FilePath
  }

data CommsThreadConfig = CommsThreadConfig
  { _commsThreadConfig_hydraNodeConfig :: HydraNodeConfig
  , _commsThreadConfig_headStatus :: TVar HydraHeadStatus
  , _commsThreadConfig_nodeStatus :: TVar HydraNodeStatus
  , _commsThreadConfig_requests :: TMVar (Map Int HydraNodeRequest)
  , _commsThreadConfig_pendingCommands :: TBQueue ClientInput
  }

makeLenses ''HydraHeadManager
makeLenses ''RunningHydraHead
makeLenses ''HydraNodeRequest
makeLenses ''HydraNode
makeLenses ''HydraNodeConfig
makeLenses ''TwoPartyHeadConfig
makeLenses ''HydraChainConfig
makeLenses ''CommsThreadConfig

-- | Should this comms thread update the Head Status?
commsThreadIsHeadStateReporter :: CommsThreadConfig -> Bool
commsThreadIsHeadStateReporter cfg =
  cfg ^. commsThreadConfig_hydraNodeConfig . hydraNodeConfig_nodeId . to (==1)
  -- ^ currently we just make the first node the state reported

-- | Transaction ID on preview from the Hydra 9.0 release
-- https://github.com/input-output-hk/hydra/releases/tag/0.9.0
previewScriptTxId :: TxId
previewScriptTxId = TxId "74b587d08d12aa679bdfeb3eaa57d698e426045dd529d4130d7d8ca0c18d54b0"

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
    let
      ready = all (== HydraNodeStatus_PeersConnected) statuses
    check ready

initHydraHead :: HydraHeadInput (Either Text ())
initHydraHead = HydraHeadInit

commitHydraHead :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadInput (Either Text ())
commitHydraHead = HydraHeadCommit

waitForApi :: Port -> IO ()
waitForApi port = do
  result :: Either IOException () <- try $ runClient "127.0.0.1" (fromIntegral port) "/" $ const $ pure ()
  case result of
    Left _ -> do
      threadDelay 100000
      waitForApi port
    Right _ -> pure ()

runHydraHead :: (MonadIO m, HasLogger a) => a -> [HydraNodeConfig] -> m RunningHydraHead
runHydraHead a configs = liftIO $ do
  headStatus <- newTVarIO HydraHead_Uninitialized
  handles <- for configs $ \config -> do
    nodeStatus <- newTVarIO HydraNodeStatus_Unavailable
    requests <- newTMVarIO mempty
    process <- createProcess <=< mkHydraNodeProc $ config
    queue <- newTBQueueIO 1000
    communicationThread <- spawnHydraNodeApiConnectionThread a $ CommsThreadConfig config headStatus nodeStatus requests queue
    pure $ Map.singleton (config ^. hydraNodeConfig_for) $ HydraNode (config ^. hydraNodeConfig_apiPort) process communicationThread nodeStatus requests queue
  pure $ RunningHydraHead headStatus $ foldOf each handles

spawnHydraNodeApiConnectionThread :: (HasLogger a, MonadIO m) => a -> CommsThreadConfig -> m ThreadId
spawnHydraNodeApiConnectionThread a cfg@(CommsThreadConfig config headStatus nodeStatus pendingRequests pendingCommands) =
  liftIO $ forkIO $ bracket (openFile (config ^. hydraNodeConfig_threadLogFile) AppendMode) hClose $ \logFile -> do
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
      forever $ do
        _ <- forkIO $ forever $ do
          cInput <- atomically $ readTBQueue pendingCommands
          logInfo a "sendingThread" $ "Sending input: " <> tShow cInput
          sendDataMessage conn $ Text (encode cInput) Nothing
        withPingThread conn 60 (pure ()) $ do
          payload <- receiveDataMessage conn
          let
            msg = case payload of
              Text v _ -> v
              Binary v -> v
            result = eitherDecode msg
          logInfo a loggerName "Got new message, logging"
          LBS.hPutStr logFile $ msg <> "\n"
          hFlush logFile
          case result of
            Right res -> do
              logInfo a loggerName $ "Valid message, processing..."
              case res of
                Greetings _ -> atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replayed
                PeerConnected _ -> do
                  logInfo a loggerName "Hydra Node ready"
                  atomically $ writeTVar nodeStatus $ HydraNodeStatus_PeersConnected
                PostTxOnChainFailed _ _ -> do
                  logInfo a loggerName "Hydra Node failed to submit transaction on chain (check for fuel)"
                HeadIsInitializing _ _ -> do
                  when (commsThreadIsHeadStateReporter cfg) $ do
                    logInfo a loggerName "Head is initializing (waiting for commits)"
                    atomically $ writeTVar headStatus $ HydraHead_Initializing
                _ -> pure ()
              handleRequests pendingRequests res
            Left err -> do
              logInfo a loggerName $ "Invalid message received: " <> tShow msg <> " " <> T.pack err
          pure ()

handleRequests :: MonadIO m => TMVar (Map Int HydraNodeRequest) -> ServerOutput -> m ()
handleRequests requests output = do
  withTMVar requests $ \current -> do
    handled <- fmap (catMaybes) $ for (Map.elems current) $ \req -> do
      case isResponse output (req ^. hydraNodeRequest_clientInput) of
        True -> do
          liftIO $ atomically $ putTMVar (req ^. hydraNodeRequest_mailbox) output
          pure $ Just $ req ^. hydraNodeRequest_id
        _ -> pure Nothing
    let
      newMap = (handled ^. traversed . to Map.delete) current
    pure (newMap, ())

isResponse :: ServerOutput -> ClientInput -> Bool
isResponse (CommandFailed offending) cInput = offending == cInput
isResponse (HeadIsInitializing _ _) Init = True
isResponse (Committed _ _ v) (Commit v') | v == v' = True
isResponse _ _ = False

unsafeAnyNode :: RunningHydraHead -> HydraNode
unsafeAnyNode = head . Map.elems . _hydraHead_handles

getNodeFor :: RunningHydraHead -> Api.AddressAny -> Maybe HydraNode
getNodeFor hHead addr = Map.lookup addr $ hHead ^. hydraHead_handles

sendHydraHeadCommand :: (MonadIO m, HasLogger a) => a -> RunningHydraHead -> HydraHeadInput b -> m b
sendHydraHeadCommand a hHead command = do
  ensureHeadNodesReady a hHead
  logInfo a "sendHydraHeadCommand" "Nodes are ready, sending command"
  case command of
    HydraHeadInit -> do
      let
        -- We don't care for init!
        node = unsafeAnyNode hHead
      mailbox <- liftIO $ newEmptyTMVarIO
      -- Submit the request
      withTMVar (node ^. hydraNode_pendingRequests) $ \current -> do
        let
          cInput = Init

          nextId = case Map.null current of
            True -> 0
            False -> fst $ Map.findMax current
          req = HydraNodeRequest nextId cInput mailbox
        liftIO $ atomically $ writeTBQueue (node ^. hydraNode_requestQueue) cInput
        pure (Map.insert nextId req current, ())
      output <- liftIO $ atomically $ takeTMVar mailbox
      case output of
        CommandFailed _ -> pure $ Left "Command Failed"
        HeadIsInitializing _ _ -> pure $ Right ()
        _ -> pure $ Left $ "Invalid response received" <> tShow output

    HydraHeadCommit addr utxo -> do
      let
        mNode = getNodeFor hHead addr
      case mNode of
        Nothing -> pure $ Left $ "Address " <> Api.serialiseAddress addr <> " is not a part of this head!"
        Just node -> runExceptT $ do
          mailbox <- liftIO $ newEmptyTMVarIO
          -- Submit the request
          withTMVar (node ^. hydraNode_pendingRequests) $ \current -> do
            let
              cInput = Commit $ Aeson.toJSON utxo

              nextId = case Map.null current of
                True -> 0
                False -> fst $ Map.findMax current
              req = HydraNodeRequest nextId cInput mailbox
            liftIO $ atomically $ writeTBQueue (node ^. hydraNode_requestQueue) cInput
            pure (Map.insert nextId req current, ())
          output <- liftIO $ atomically $ takeTMVar mailbox
          case output of
            CommandFailed _ -> throwError "Command Failed"
            Committed _ _ _ -> pure ()
            _ -> do
              throwError $ "Invalid response received" <> tShow output


logTo :: Handle -> Handle -> CreateProcess -> CreateProcess
logTo out err cp =
  cp { std_out = UseHandle out
     , std_err = UseHandle err
     }

mkHydraNodeProc :: MonadIO m => HydraNodeConfig -> m CreateProcess
mkHydraNodeProc cfg = do
  out <- liftIO $ openFile (cfg ^. hydraNodeConfig_logFile) AppendMode
  err <- liftIO $ openFile (cfg ^. hydraNodeConfig_logErrFile) AppendMode
  pure $ logTo out err $ proc hydraNodePath $ join
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
     , "--network-id"
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

deriveConfigFromDbHead :: (MonadIO m, HasPortRange a, HasLogger a, HasNodeInfo a, Db.HasDbConnectionPool a) => a -> Db.HydraHead -> m (Either Text [HydraNodeConfig])
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

  firstProxy <- ExceptT $ queryProxyInfo a first
  secondProxy <- ExceptT $ queryProxyInfo a second

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

queryHydraHeadConfigs :: (MonadIO m, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a, Db.HasDbConnectionPool a) => a -> Int32 -> m (Either Text [HydraNodeConfig])
queryHydraHeadConfigs a hid = do
  mHead <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ do
    runSelectReturningOne $ select $ do
      h <- all_ (Db.db ^. Db.db_heads)
      guard_ (val_ (Db.HeadID $ SqlSerial hid) `references_` h)
      pure h

  case mHead of
    Nothing -> pure $ Left $ "Invalid head " <> tShow hid <> " was not found"
    Just hh -> deriveConfigFromDbHead a hh

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

withHydraHeadManager :: (HydraHeadManager -> IO a) -> IO a
withHydraHeadManager action = do
  bracket newHydraHeadManager terminateRunningHeads action

startupExistingHeads :: (MonadIO m, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a, Db.HasDbConnectionPool a) => a -> m (Either Text Int)
startupExistingHeads a = do
  allHeads <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ do
    runSelectReturningList $ select $ all_ (Db.db ^. Db.db_heads)
  results <- for allHeads $ \dbHead -> runExceptT $ do
    nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
    hydraHead <- lift $ runHydraHead a nodeConfigs
    let
      headId = Db.hydraHeadId dbHead
    ExceptT $ trackRunningHead a headId hydraHead
  pure $ fmap (sum . fmap (const 1)) $ sequenceA results

newHydraHeadManager :: MonadIO m => m HydraHeadManager
newHydraHeadManager = HydraHeadManager <$> (liftIO . newTMVarIO) mempty

terminateRunningHeads :: MonadIO m => HydraHeadManager -> m ()
terminateRunningHeads (HydraHeadManager channels) = do
  withTMVar channels $ \running -> do
    for_ (Map.elems running) $ \headVar -> do
      withTMVar headVar $ \hydraHead@(RunningHydraHead _ handles) -> do
        for_ handles $ \(HydraNode _ nodeHandle@(_, out, err, _) thread _ _ _) -> liftIO $ do
          cleanupProcess nodeHandle
          maybe (pure ()) hClose out
          maybe (pure ()) hClose err
          killThread thread
        pure (hydraHead, ())
    pure (running, ())

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

commitToHead :: (MonadIO m, HasNodeInfo a, HasLogger a, Db.HasDbConnectionPool a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> Api.Lovelace -> m (Either Text ())
commitToHead a hid committer amount = do
  result <- runExceptT $ do
    info <- ExceptT $ queryProxyInfo a committer
    balance <- ExceptT $ runCardanoCli a $ queryUTxOs committer
    commitUtxo <- ExceptT $ pure $ maybeToEither ("Failed to find suitable commit utxo with " <> tShow amount <> "lovelace") $ findUTxOWithExactly amount balance
    logInfo a "commitHead" $ "Attempting to Commit to " <> tShow hid
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand a hydraHead $ commitHydraHead committer commitUtxo
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "commitHead" $ "Head " <> tShow hid <> " was committed to by " <> Api.serialiseAddress committer
    Left err -> logInfo a "commitHead" $ "Head failed to commit from " <> Api.serialiseAddress committer <> ": " <> err
  pure result
