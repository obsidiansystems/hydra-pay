{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra where

import Debug.Trace (traceM, traceShow)

import System.IO
import System.Which
import System.Process
import System.FilePath
import System.Directory
import System.IO.Temp

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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Proxy
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Hydra.ChainConfig
import HydraPay.Cardano.Node
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
import Control.Exception (IOException)
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LBS

import Network.WebSockets (runClient, sendDataMessage, withPingThread)
import qualified Network.WebSockets as WS

data HydraHeadManager = HydraHeadManager
  { _hydraHeadManager_runningHeads :: TMVar (Map HeadId (TMVar RunningHydraHead))
  }

class HasHydraHeadManager a where
  hydraHeadManager :: Lens' a HydraHeadManager

instance HasHydraHeadManager HydraHeadManager where
  hydraHeadManager = id

-- TODO(skylar): Should this map directly to the Hydra Primitives? Probably
data HydraHeadCommand :: * -> * where
  Command_Init :: HydraHeadCommand (Either Text HeadId)
  Command_Commit :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadCommand (Either Text ())
  Command_Balances :: HydraHeadCommand (Either Text (Api.UTxO Api.BabbageEra))
  Command_NewTx :: Api.AddressAny -> Text -> HydraHeadCommand (Either Text ())
  Command_Close :: HydraHeadCommand (Either Text ())
  Command_Fanout :: HydraHeadCommand (Either Text ())

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
  , _hydraNodeConfig_for :: Api.AddressAny
  }

data TwoPartyHeadConfig = TwoPartyHeadConfig
  { _twoPartyHeadConfig_firstParty :: ProxyInfo
  , _twoPartyHeadConfig_secondParty :: ProxyInfo
  , _twoPartyHeadConfig_persistDirectory :: FilePath
  }

data CommsThreadConfig = CommsThreadConfig
  { _commsThreadConfig_hydraNodeConfig :: HydraNodeConfig
  , _commsThreadConfig_headStatus :: TVar HydraHeadStatus
  , _commsThreadConfig_nodeStatus :: TVar HydraNodeStatus
  , _commsThreadConfig_requests :: TMVar (Map Int HydraNodeRequest)
  , _commsThreadConfig_pendingCommands :: TBQueue ClientInput
  , _commsThreadConfig_nodeProcess :: ProcessHandle
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
mkTwoPartyHydraNodeConfigs a scriptsTxId prange (HydraChainConfig genesis pparams) (TwoPartyHeadConfig p1 p2 persistDir) = runExceptT $ do
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
             (persistDir </> "node1")
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
             (persistDir </> "node2")
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
      ready = all (\a -> a == HydraNodeStatus_PeersConnected) statuses
    check ready

commandInit :: HydraHeadCommand (Either Text HeadId)
commandInit = Command_Init

commandCommit :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> HydraHeadCommand (Either Text ())
commandCommit = Command_Commit

commandClose :: HydraHeadCommand (Either Text ())
commandClose = Command_Close

commandFanout :: HydraHeadCommand (Either Text ())
commandFanout = Command_Fanout

commandBalances :: HydraHeadCommand (Either Text (Api.UTxO Api.BabbageEra))
commandBalances = Command_Balances

commandNewTx :: Api.AddressAny -> Text -> HydraHeadCommand (Either Text ())
commandNewTx = Command_NewTx

waitForApi :: Port -> IO ()
waitForApi port = do
  result :: Either IOException () <- try $ runClient "127.0.0.1" (fromIntegral port) "/" $ const $ pure ()
  case result of
    Left _ -> do
      threadDelay 100000
      waitForApi port
    Right _ -> pure ()

-- TODO(skylar): This isn't really running a head but it is... Naming naming naming
runHydraHead :: (MonadIO m, HasLogger a, HasNodeInfo a, Db.HasDbConnectionPool a) => a -> [HydraNodeConfig] -> m RunningHydraHead
runHydraHead a configs = liftIO $ do
  headStatus <- newTVarIO HydraHead_Uninitialized
  handles <- for configs $ \config -> do
    nodeStatus <- newTVarIO HydraNodeStatus_Unavailable
    requests <- newTMVarIO mempty
    process <- createProcess <=< mkHydraNodeProc $ config
    queue <- newTBQueueIO 1000
    communicationThread <- spawnHydraNodeApiConnectionThread a $ CommsThreadConfig config headStatus nodeStatus requests queue $ processInfoHandle process
    broadcastChan <- newBroadcastTChanIO
    pure $ Map.singleton (config ^. hydraNodeConfig_for) $ HydraNode (config ^. hydraNodeConfig_apiPort) process communicationThread nodeStatus queue broadcastChan
  pure $ RunningHydraHead headStatus $ foldOf each handles

spawnHydraNodeApiConnectionThread :: (HasLogger a, HasNodeInfo a, Db.HasDbConnectionPool a, MonadIO m) => a -> CommsThreadConfig -> m ThreadId
spawnHydraNodeApiConnectionThread a cfg@(CommsThreadConfig config headStatus nodeStatus pendingRequests pendingCommands processHandle) =
  liftIO $ forkIO $ forever $ do
    logInfo a loggerName "Waiting for API to be available"

    -- Check the node process and restart it if it has died, this should prevent outer API calls for hitting the node.
    -- TODO(skylar): Trigger that the Head is effectively dead, probably reset state, and then probably just
    -- try and restart things maybe eventually?
    result <- getProcessExitCode processHandle
    case result of
      Just _ -> do
        logError a loggerName "Node process has died!"
        -- TODO(skylar): Restart the nodes if they fail!
      Nothing -> pure ()

    waitForApi apiPort
    logInfo a loggerName "API available connecting..."
    runClient "127.0.0.1" (fromIntegral apiPort) "/" $ \conn -> do
      -- We are connected so the nodes will be "replaying"
      atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replaying
      requestThreadId <- forkIO $ forever $ do
        cInput <- atomically $ readTBQueue pendingCommands
        logInfo a "sendingThread" $ "Sending input: " <> tShow cInput
        sendDataMessage conn $ WS.Text (encode cInput) Nothing
      -- TODO(skylar): This is kind of jank the threads should be managed better aka die correctly
      flip finally (killThread requestThreadId) $ withPingThread conn 60 (pure ()) $ forever $ do
        result <- try $ do
          payload <- WS.receiveData conn
          logInfo a loggerName "Got new message, logging"
          case eitherDecode payload of
            Right res -> do
              logInfo a loggerName $ "Valid message, processing...\n" <> tShow res
              handleHydraNodeApiResponse loggerName isReporter res
              handleRequests pendingRequests res
            Left err -> do
              logInfo a loggerName $ "Invalid message received: " <> tShow payload <> " " <> T.pack err
        case result of
          Left err@(SomeException _) ->
            logInfo a loggerName $ "Message Handler failed: " <> tShow err
          Right _ ->
            pure ()
  where
    apiPort = config ^. hydraNodeConfig_apiPort
    isReporter = commsThreadIsHeadStateReporter cfg
    loggerName = bool "commsThread" "reporterThread" isReporter

    -- Update hydra-pay state based on hydra node responses
    handleHydraNodeApiResponse loggerName isReporter = \case
      Greetings {} ->
        atomically $ writeTVar nodeStatus $ HydraNodeStatus_Replayed
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
          when (status /= HydraNodeStatus_Replaying) $ do
            liftIO $ atomically $ writeTBQueue pendingCommands Fanout
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

handleRequests :: MonadIO m => TMVar (Map Int HydraNodeRequest) -> ServerOutput -> m ()
handleRequests requests output = do
  traceM $ "handleRequests: " <> show output
  withTMVar requests $ \current -> do
    handled <- fmap (catMaybes) $ for (Map.elems current) $ \req -> do
      case isRelatedOutput output (req ^. hydraNodeRequest_clientInput) of
        True -> do
          liftIO $ atomically $ putTMVar (req ^. hydraNodeRequest_mailbox) output
          pure $ Just $ req ^. hydraNodeRequest_id
        False -> do
          traceM  $ "handleRequests: Not response: " <> (show output)
          pure Nothing
    let
      newMap = (handled ^. traversed . to Map.delete) current
    pure (newMap, ())

-- | This maps the various potential outputs with the input
-- TODO(skylar): At some point we care about PostTxOnChainFailed but how can we
-- figure out why that happened and who is responsible?
-- PostTxOnChainFailed does give us the transaction that failed, but that is kind of an internal thing
-- we can't really scrutinize the transaction because we aren't the ones who created or submitted it
-- it is purely a implementation detail of Hydra and one we would have to dig into to act on.
-- I guess if the node gives us this we should just requeue the last action...
-- Maybe we can use the sequence number??? Like can we keep track of the last seen thing
-- and then when we submit we should take the last seen one and consider the PostTxFailed if it is
-- greater than that last seq id?
-- NOTE(skylar): We don't care about TxValid because TxValid says a transaction is valid, not that it
-- submitted successfully and is seend by the nodes
isRelatedOutput :: ServerOutput -> ClientInput -> Bool
isRelatedOutput (CommandFailed offending) cInput =
  traceShow
  (offending, cInput)
  (offending == cInput)
isRelatedOutput (HeadIsInitializing _ _) Init = True
isRelatedOutput (Committed _ _ v) (Commit v') | v == v' = True
isRelatedOutput (GetUTxOResponse {}) GetUTxO =
  -- TODO(skylar): We may get wrong results if multiple of GetUTxO are getting sent
  -- though this may not matter as this is eventually consistent...
  True
isRelatedOutput (TxInvalid _ _ invalidTx _) (NewTx sentTx) =
  -- We care about this TxInvalid if it is about the tx we sent
  invalidTx == sentTx
isRelatedOutput (SnapshotConfirmed _ snap _) (NewTx sentTx) =
  -- We care about this snapshot confirmed if it confirms our transaction
  any (==sentTx) (snapshot_confirmedTransactions snap)
isRelatedOutput (HeadIsClosed {}) Close = True
-- TODO(skylar): Is this a bug?
isRelatedOutput (Committed {}) Fanout = True
isRelatedOutput (HeadIsFinalized {}) Fanout = True
isRelatedOutput _ _ = False

unsafeAnyNode :: RunningHydraHead -> HydraNode
unsafeAnyNode = head . Map.elems . _hydraHead_handles

lookupNodeFor :: RunningHydraHead -> Api.AddressAny -> Maybe HydraNode
lookupNodeFor hHead addr = Map.lookup addr $ hHead ^. hydraHead_handles

getNodeFor :: MonadError Text m => RunningHydraHead -> Api.AddressAny -> m HydraNode
getNodeFor hHead addr = do
  case lookupNodeFor hHead addr of
    Nothing -> throwError $ "Address " <> Api.serialiseAddress addr <> " is not a part of this head!"
    Just node ->  pure node

sendInputAndAwaitEffect :: MonadIO m => HydraNode -> ClientInput -> m ServerOutput
sendInputAndAwaitEffect node command = liftIO $ do
  atomically $ do
    chan <- dupTChan (node ^. hydraNode_broadcastChannel)
    -- Queue the command
    writeTBQueue (node ^. hydraNode_clientInputQueue) command
    -- Wait for the response
    readBroadcastChan chan ((flip isRelatedOutput) command)

-- TODO(skylar): This is pretty 'low-level and shouldn't be usable outside the Head Manager'
-- | Send a command to a Hydra Head
sendCommand :: (MonadIO m, HasLogger a) => a -> RunningHydraHead -> HydraHeadCommand b -> m b
sendCommand a hHead command = do
  ensureHeadNodesReady a hHead
  logInfo a "sendCommand" "Nodes are ready, sending command"
  case command of
    Command_Init -> do
      output <- sendInputAndAwaitEffect (unsafeAnyNode hHead) Init
      case output of
        HeadIsInitializing hid _ -> pure $ Right hid
        _ -> pure $ Left $ "Failed to initialize head, invalid response received: " <> tShow output

    Command_Commit addr commitUtxo -> runExceptT $ do
      node <- getNodeFor hHead addr
      output <- sendInputAndAwaitEffect node $ Commit $ Aeson.toJSON commitUtxo
      case output of
        CommandFailed cm -> throwError $ "Commit Failed: " <> T.pack (show cm)
        Committed _ _ _ -> pure ()
        _ -> throwError $ "Invalid response received" <> tShow output

    Command_Balances -> runExceptT $ do
      let node = unsafeAnyNode hHead
      output <- sendInputAndAwaitEffect node GetUTxO
      case output of
        GetUTxOResponse _ utxoJson -> do
          case Aeson.fromJSON utxoJson of
            Aeson.Success balances -> pure balances
            Aeson.Error e -> throwError $ "Failed to decode GetUTxOResponse: " <> T.pack e
        CommandFailed _ ->
          throwError "Failed to get balances at this time"
        _ ->
          throwError $ "Invalid response received: " <> tShow output

    Command_NewTx addr txCBOR -> runExceptT $ do
      node <- getNodeFor hHead addr
      let
        txValue = Aeson.String txCBOR
      output <- sendInputAndAwaitEffect node $ NewTx txValue
      case output of
        -- NOTE(skylar): We don't care about TxValid because TxValid says a transaction is valid, not that it
        -- submitted successfully and is seend by the nodes
        TxInvalid {} -> throwError $ "Transaction is invalid"
        SnapshotConfirmed _ _ _ -> pure ()

    Command_Close -> runExceptT $ do
      pure ()

    Command_Fanout -> runExceptT $ do
      pure ()
    {-
       node <- case getNodeFor hHead addr of
         Nothing -> throwError $ "Address " <> Api.serialiseAddress addr <> " is not a part of this head!"
         Just node ->  pure node
       output <- performHydraNodeRequest node $ NewTx $ Aeson.String txCBOR
       case output of
         TxValid {} -> do
           traceM $ "NewTx: " <> show output
           pure ()
         SnapshotConfirmed _ snapshotJson _ -> do
           traceM $ "NewTx: SnapshotConfirmed: " <> show snapshotJson
         TxInvalid _ _ _ _ -> throwError "NewTx Failed"
         _ -> do
           traceM $ "Invalid response received" <> show output
           throwError $ "Invalid response received" <> tShow output-}
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

-- sendCommand :: (MonadIO m, HasLogger a) => a -> RunningHydraHead -> HydraHeadCommand b -> m b
-- sendCommand a hHead command = do
--   ensureHeadNodesReady a hHead
--   logInfo a "sendCommand" "Nodes are ready, sending command"
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

filterUTxOByAddress :: Api.AddressAny -> Api.UTxO Api.BabbageEra -> Api.UTxO Api.BabbageEra
filterUTxOByAddress addr (Api.UTxO m) = Api.UTxO $ Map.filter (\x -> txOutAddress x == addr) m
  where
    txOutAddress (Api.TxOut (Api.AddressInEra _ a) _ _ _) = Api.toAddressAny a

-- | Read messages coming through the websocket that match the filter, will wait if no messages are available on the queue
readBroadcastChan :: TChan a -> (a -> Bool) -> STM a
readBroadcastChan chan filterFunc = do
  churn chan
  where
    churn chan = do
      output <- readTChan chan
      case filterFunc output of
        True -> pure output
        False -> churn chan

-- Performing a request on a node consists on adding it to a queue which
-- eventually is processed and communicates through a websocket with the hydra
-- node and awaits for the response to get sent back.
-- performHydraNodeRequest :: MonadIO m => HydraNode -> ClientInput -> m ServerOutput
-- performHydraNodeRequest node i = do
--   -- Where the result will be stored
--   mailbox <- liftIO newEmptyTMVarIO
--   -- Submit the request
--   withTMVar (node ^. hydraNode_pendingRequests) $ \current -> do
--     let
--       nextId = case Map.lookupMax current of
--         Nothing -> 0
--         Just (k, _) -> k
--       req = HydraNodeRequest nextId i mailbox
--     liftIO $ atomically $ writeTBQueue (node ^. hydraNode_requestQueue) i
--     pure (Map.insert nextId req current, ())
--   liftIO $ atomically $ takeTMVar mailbox

logTo :: Handle -> Handle -> CreateProcess -> CreateProcess
logTo out err cp =
  cp { std_out = UseHandle out
     , std_err = UseHandle err
     }

mkHydraNodeProc :: MonadIO m => HydraNodeConfig -> m CreateProcess
mkHydraNodeProc cfg = do
  -- out <- liftIO $ openFile (cfg ^. hydraNodeConfig_logFile) AppendMode
  -- err <- liftIO $ openFile (cfg ^. hydraNodeConfig_logErrFile) AppendMode
  pure $ proc hydraNodePath $ join
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

  firstProxy <- ExceptT $ queryProxyInfo a first
  secondProxy <- ExceptT $ queryProxyInfo a second

  let
    headConfig = TwoPartyHeadConfig
      firstProxy
      secondProxy
      undefined

  nodeConfigs <- ExceptT $ mkTwoPartyHydraNodeConfigs a previewScriptTxId (a ^. portRange) chain headConfig
  pure nodeConfigs

headPersistDir :: FilePath
headPersistDir = "hydra-head-persistence"

headNodeLogsDir :: FilePath
headNodeLogsDir = "hydra-node-logs"

getRunningHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> HeadId -> m (Either Text (TMVar RunningHydraHead))
getRunningHead a hid = do
  logInfo a "getRunningHead" $ "Fetching head " <> tShow hid
  liftIO $ withTMVar runningHeads $ \running -> do
    traceM $ "RUNNING => " <> show (Map.keys running)
    case Map.lookup hid running of
      Just h -> pure (running, Right h)
      Nothing -> pure (running, Left $ "Running head with id " <> tShow hid <> " couldn't be found")
  where
    runningHeads = a ^. hydraHeadManager . hydraHeadManager_runningHeads

trackRunningHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> HeadId -> RunningHydraHead -> m (Either Text ())
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
    hydraHead <- lift $ runHydraHead a nodeConfigs
    -- let
      -- headId = Db.hydraHeadId dbHead
    ExceptT $ trackRunningHead a undefined hydraHead
  pure $ fmap (sum . fmap (const 1)) $ sequenceA results

activeHydraHeads :: MonadBeam Postgres m => m [Db.HydraHead]
activeHydraHeads = do
  pure []
  {-
  runSelectReturningList $ select $ do
    heads_ <- all_ (Db.db ^. Db.db_heads)
    paymentChan_ <- join_ (Db.db ^. Db.db_paymentChannels) (\paymentChan -> (paymentChan ^. Db.paymentChannel_head) `references_` heads_)
    guard_ (paymentChan_ ^. Db.paymentChannel_open)
    pure $ heads_-}

spinUpHead :: (MonadIO m, MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, Db.HasDbConnectionPool a, HasLogger a, HasNodeInfo a, HasPortRange a, HasHydraHeadManager a) => a -> Int32 -> m (Either Text ())
spinUpHead a hid = runExceptT $ do
  pure undefined
  {-
  mHead <- do
    runSelectReturningOne $ select $ do
      h <- all_ (Db.db ^. Db.db_heads)
      guard_ (h ^. Db.hydraHead_id ==. val_ (SqlSerial hid))
      pure h
  case mHead of
    Nothing -> throwError $ "Invalid head id" <> tShow hid
    Just dbHead -> do
      traceM $ "spinUpHead: Foudn head: " <>  show (Db._hydraHead_id dbHead)
      nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
      traceM $ "spinUpHead: Configs retrieved"
      hydraHead <- lift $ runHydraHead a nodeConfigs
      traceM $ "spinUpHead: Ran Hydra Head"
      out <- ExceptT $ trackRunningHead a hid hydraHead
      traceM $ "spinUpHead: Tracked Running Head"
      pure out-}

newHydraHeadManager :: MonadIO m => m HydraHeadManager
newHydraHeadManager = HydraHeadManager <$> (liftIO . newTMVarIO) mempty

createHead :: (MonadMask m, MonadIO m, HasLogger a, HasNodeInfo a, HasPortRange a, Db.HasDbConnectionPool a, HasHydraHeadManager a, MonadError Text m) => a -> HydraChainConfig -> Set Api.AddressAny -> m HeadId
createHead a chainCfg participants = do
  logInfo a "createHead" "Look I did a thing"
  withTempDirectory "." "initializing-head" $ \persistDir -> do
    runningHeadResult <- runExceptT $ do

      logInfo a "createHead" "Shall we fail?"
      -- Make sure this head is feasible in participant list
      when (Set.size participants == 2) $ throwError "We only handle heads with two participants"

      logInfo a "createHead" "Getting proxies"

      firstProxy <- ExceptT $ Db.runBeam a $ queryProxyInfo a first
      secondProxy <- ExceptT $ Db.runBeam a $ queryProxyInfo a second

      nodeConfigs <- ExceptT $ mkTwoPartyHydraNodeConfigs a previewScriptTxId (a ^. portRange) chainCfg $ TwoPartyHeadConfig firstProxy secondProxy persistDir
      runningHead <- runHydraHead a nodeConfigs

      logInfo a "createHead" $ "Running new head in " <> T.pack persistDir

      pure (runningHead, firstProxy, secondProxy)

    -- NOTE(skylar): We want to be able to terminate the Head so we need to have a reference to the RunningHydraHead to do so.
    -- that makes these case statements a little messy but necessary.
    case runningHeadResult of
      Right (runningHead, firstProxy, secondProxy) -> do
        logInfo a "createHead" $ "New head is running, asking to Init"
        commandResult <- sendCommand a runningHead commandInit

        -- We are going to move everything out of the directory, kill the nodes first!
        killRunningHead runningHead

        case commandResult of
          Right headid -> do
            logInfo a "createHead" $ "Head initialized " <> headid <> " persisting..."

            Db.runBeam a $ do
              -- We have a HeadId so now we can persist the Head
              runInsertReturningList $ insert (Db.db ^. Db.db_heads) $
                insertExpressions
                  [ Db.HydraHead
                    (val_ headid)
                    (val_ firstText)
                    (val_ secondText)
                    nothing_
                    nothing_
                    (val_ $ chainCfg ^. hydraChainConfig_ledgerGenesis . to T.pack)
                    (val_ $ chainCfg ^. hydraChainConfig_ledgerProtocolParams . to T.pack)
                  ]

            -- We can also create some more permanent head persistence because of the headid
            let
              newHeadDirectory = "heads" </> T.unpack headid
            liftIO $ createDirectoryIfMissing True newHeadDirectory

            logInfo a "createHead" $ "Copying over node persist info into " <> headid
            copyDirectory persistDir newHeadDirectory

            result <- runExceptT $ do
              logInfo a "createHead" "Restarting and tracking Head"
              nodeConfigs <- ExceptT $ mkTwoPartyHydraNodeConfigs a previewScriptTxId (a ^. portRange) chainCfg $ TwoPartyHeadConfig firstProxy secondProxy newHeadDirectory
              finalHead <- runHydraHead a nodeConfigs
              trackRunningHead a headid finalHead
              pure headid
            case result of
              Right headid -> do
                logInfo a "createHead" "Head complete"
                pure headid
              Left err -> do
                logInfo a "createHead" "Failed to restart new Head"
                throwError err

          Left err -> do
            logInfo a "createHead" "Failed to create temp head"
            throwError err
  where
    [first, second] = Set.toList participants
    firstText = Api.serialiseAddress first
    secondText = Api.serialiseAddress second

copyDirectory :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectory fromDir toDir = liftIO $ do
  createDirectoryIfMissing True toDir

  files <- listDirectory fromDir

  for files $ \file -> do
    isDirectory <- doesDirectoryExist file
    case isDirectory of
      -- Copy files
      False -> copyFile file (replaceDirectory toDir file)
      -- Traverse directories
      True -> copyDirectory file (replaceDirectory toDir file)
  pure ()

terminateRunningHeads :: MonadIO m => HydraHeadManager -> m ()
terminateRunningHeads (HydraHeadManager channels) = do
  withTMVar channels $ \running -> do
    for_ (Map.elems running) $ \headVar -> do
      withTMVar headVar $ \hydraHead@(RunningHydraHead _ handles) -> do
        for_ handles $ \(HydraNode _ nodeHandle@(_, out, err, _) thread _ _ _) -> liftIO $ do
          cleanupProcess nodeHandle
          killThread thread
        pure (hydraHead, ())
    pure (running, ())

killRunningHead :: MonadIO m => RunningHydraHead -> m ()
killRunningHead hydraHead@(RunningHydraHead _ handles) = do
  for_ handles $ \(HydraNode _ nodeHandle@(_, out, err, _) thread _ _ _) -> liftIO $ do
    cleanupProcess nodeHandle
    killThread thread

initHead :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> m (Either Text HeadId)
initHead a hid = do
  pure undefined
  {-
  logInfo a "initHead" $ "Attempting to Init head " <> tShow hid
  result <- runExceptT $ do
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendCommand a hydraHead commandInit

      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "initHead" $ "Head " <> tShow hid <> " is initialized"
    Left err -> logInfo a "initHead" $ "Head failed to initialize: " <> err
  pure result-}

commitToHead :: (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m,  MonadIO m, HasNodeInfo a, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> Api.Lovelace -> m (Either Text ())
commitToHead a hid committer amount = do
  pure undefined
  {-
  result <- runExceptT $ do
    proxyAddr <- fmap _proxyInfo_address $ ExceptT $ queryProxyInfo a committer
    logInfo a "commitHead" $ "Attempting to Commit from address: " <> Api.serialiseAddress proxyAddr
    balance <- ExceptT $ runCardanoCli a $ queryUTxOs proxyAddr
    commitUtxo <- ExceptT $ pure $ maybeToEither ("Failed to find suitable commit utxo with " <> tShow amount <> "lovelace") $ findUTxOWithExactly amount balance
    logInfo a "commitHead" $ "Attempting to Commit to " <> tShow hid
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendCommand a hydraHead $ commandCommit proxyAddr commitUtxo
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "commitHead" $ "Head " <> tShow hid <> " was committed to by " <> Api.serialiseAddress committer
    Left err -> logInfo a "commitHead" $ "Head failed to commit from " <> Api.serialiseAddress committer <> ": " <> err
  pure result -}

getHeadBalance :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> m (Either Text (Api.UTxO Api.BabbageEra))
getHeadBalance a hid addr = do
  pure $ Right $ Api.UTxO mempty
  {-
  runExceptT $ do
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      balances <- sendCommand a hydraHead $ commandBalances
      pure (hydraHead, balances)-}

newTx :: (MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> Text -> Text -> m (Either Text ())
newTx a hid addr _txid txCBOR = do
  pure undefined
  {-
  runExceptT $ do
    traceM "newTx: Start"
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      traceM "newTx: Pre cmd"
      result <- sendCommand a hydraHead $ commandNewTx addr txCBOR
      traceM "newTx: Post cmd"
      pure (hydraHead, result)-}

closeHead :: (MonadBeam Postgres m, MonadIO m, HasLogger a, HasHydraHeadManager a) => a -> Int32 -> Api.AddressAny -> m (Either Text ())
closeHead a hid committer = do
  pure undefined
  {-
  result <- runExceptT $ do
    logInfo a "closeHead" $ "Attempting to Close " <> tShow hid
    hydraHeadVar <- ExceptT $ getRunningHead a hid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendCommand a hydraHead commandClose
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "closeHead" $ "Head " <> tShow hid <> " was closed by " <> Api.serialiseAddress committer
    Left err -> logInfo a "closeHead" $ "Head failed to commit from " <> Api.serialiseAddress committer <> ": " <> err
  pure result-}
