{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra where

import System.IO
import System.Which
import System.Process

import Data.Int
import Data.Word
import Data.Maybe
import Data.Text (Text)
import Data.Set (Set)
import Data.Traversable
import Data.Aeson (encode, eitherDecode)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as Map

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Proxy
import HydraPay.Logging
import HydraPay.Cardano.Node
import HydraPay.PortRange
import HydraPay.Cardano.Hydra.Api

import Control.Lens
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Cardano.Api as Api

import Network.WebSockets

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
  }

makeLenses ''HydraNodeConfig

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

makeLenses ''TwoPartyHeadConfig

data HydraChainConfig = HydraChainConfig
  { _hydraChainConfig_ledgerGenesis :: FilePath
  , _hydraChainConfig_ledgerProtocolParams :: FilePath
  }

makeLenses ''HydraChainConfig

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
  let
    [firstPort, firstApiPort, firstMonitorPort, secondPort, secondApiPort, secondMonitorPort] = allocatedPorts pa
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
       ]
  where
    ninfo = a ^. nodeInfo

-- | A running Hydra Head
data HydraHead = HydraHead
  { _hydraHead_handles :: [HydraNode]
  }

data HydraNodeStatus =
  HydraNodeStatus_Replaying | HydraNodeStatus_Replayed | HydraNodeStatus_PeersConnected

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

type ProcessInfo = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

makeLenses ''HydraNodeRequest
makeLenses ''HydraNode

data HydraHeadInput :: * -> * where
  HydraHeadInit :: HydraHeadInput (Either Text ())

initHydraHead :: HydraHeadInput (Either Text ())
initHydraHead = HydraHeadInit

waitForApi :: Port -> IO ()
waitForApi port = do
  result :: Either IOException () <- try $ runClient "127.0.0.1" (fromIntegral port) "/" $ const $ pure ()
  case result of
    Left _ -> do
      threadDelay 100000
      waitForApi port
    Right _ -> pure ()

runHydraHead :: (MonadIO m, HasLogger a) => a -> [HydraNodeConfig] -> m HydraHead
runHydraHead a configs = liftIO $ do
  handles <- for configs $ \config -> do
    status <- newTVarIO HydraNodeStatus_Replaying
    requests <- newTMVarIO mempty
    process <- createProcess <=< mkHydraNodeProc $ config
    queue <- newTBQueueIO 1000
    communicationThread <- spawnHydraNodeApiConnectionThread a config status requests queue
    pure $ HydraNode (config ^. hydraNodeConfig_apiPort) process communicationThread status requests queue
  pure $ HydraHead $ handles ^.. each

spawnHydraNodeApiConnectionThread :: (HasLogger a, MonadIO m) => a -> HydraNodeConfig -> TVar HydraNodeStatus -> TMVar (Map Int HydraNodeRequest) -> TBQueue ClientInput-> m ThreadId
spawnHydraNodeApiConnectionThread a config status pendingRequests inputs =
  liftIO $ forkIO $ bracket (openFile (config ^. hydraNodeConfig_threadLogFile) AppendMode) hClose $ \logFile -> do
    let
      apiPort = config ^. hydraNodeConfig_apiPort
    logInfo a "communicationThread" "Waiting for API to be available"
    waitForApi apiPort
    logInfo a "communicationThread" "API available connecting..."
    runClient "127.0.0.1" (fromIntegral apiPort) "/" $ \conn -> forever $ do
      forkIO $ forever $ do
        input <- atomically $ readTBQueue inputs
        logInfo a "sendingThread" $ "Sending input: " <> tShow input
        sendDataMessage conn $ Text (encode input) Nothing
      withPingThread conn 60 (pure ()) $ do
        payload <- receiveDataMessage conn
        let
          msg = case payload of
            Text v _ -> v
            Binary v -> v
          result = eitherDecode msg
        logInfo a "communicationThread" "Got new message, logging"
        LBS.hPutStr logFile $ msg <> "\n"
        hFlush logFile
        case result of
          Right res -> do
            logInfo a "communicationThread" $ "Valid message, processing..."
            case res of
              Greetings _ -> atomically $ writeTVar status $ HydraNodeStatus_Replayed
              PeerConnected _ -> atomically $ writeTVar status $ HydraNodeStatus_PeersConnected
              _ -> pure ()
            handleRequests pendingRequests res
          Left err -> do
            logInfo a "communicationThread" $ "Invalid message received: " <> tShow msg <> " " <> T.pack err
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
isResponse (CommandFailed offending) input = offending == input
isResponse (HeadIsInitializing _ _) Init = True
isResponse _ _ = False

sendHydraHeadCommand :: MonadIO m => HydraHead -> HydraHeadInput a -> m a
sendHydraHeadCommand (HydraHead (node:_)) = \case
  HydraHeadInit -> do
    mailbox <- liftIO $ newEmptyTMVarIO
    -- Submit the request
    withTMVar (node ^. hydraNode_pendingRequests) $ \current -> do
      let
        input = Init

        nextId = case Map.null current of
          True -> 0
          False -> fst $ Map.findMax current
        req = HydraNodeRequest nextId input mailbox
      liftIO $ atomically $ writeTBQueue (node ^. hydraNode_requestQueue) input
      pure (Map.insert nextId req current, ())
    output <- liftIO $ atomically $ takeTMVar mailbox
    case output of
      CommandFailed _ -> pure $ Left "Command Failed"
      HeadIsInitializing _ _ -> pure $ Right ()
      _ -> pure $ Left $ "Invalid response received" <> tShow output

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
