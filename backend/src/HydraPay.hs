-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module HydraPay where

import Prelude hiding ((.))
import Control.Category ((.))
import System.Process
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Bool
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Aeson ((.:))
import Data.Aeson as Aeson
import Data.Maybe
import Data.String.Interpolate ( i, iii )
import System.IO (IOMode(WriteMode), openFile)
import Network.WebSockets.Client
import qualified Network.WebSockets.Connection as WS

import Control.Concurrent
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.Compat (nominalDiffTimeToSeconds)
import Data.Fixed

import Data.Text.Prettyprint.Doc
import Control.Monad.Log
import System.Directory

import Data.Traversable

import Control.Monad

import Hydra.Types
import Hydra.Devnet
import Hydra.ClientInput

import HydraPay.Api

import qualified Hydra.Types as HT
import CardanoNodeInfo
import Hydra.ServerOutput as ServerOutput
import Control.Concurrent.STM (newBroadcastTChanIO, dupTChan, TChan)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Monad.Loops (untilJust, iterateUntilM)
import Data.Aeson.Types (parseMaybe)
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))


-- | The location where we store cardano and hydra keys
getKeyPath :: IO FilePath
getKeyPath = do
  createDirectoryIfMissing True path
  pure path
  where
    path = "keys"

withLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
withLogging = flip runLoggingT (print . renderWithSeverity id)

-- | State we need to run/manage Heads
data State = State
  { _state_hydraInfo :: HydraSharedInfo
  , _state_proxyAddresses :: MVar (Map Address (Address, HydraKeyInfo))
  -- ^ This is really temporary it has the mapping from cardano address to proxy + keys
  , _state_heads :: MVar (Map HeadName Head)
  -- ^ This is really temporary until we stuff it all in a database
  , _state_networks :: MVar (Map HeadName Network)
  -- , _state_connectionPool :: Pool Connection -- We could ignore htis for now
  , _state_keyPath :: FilePath
  }

-- | A Hydra Head in Hydra Pay
data Head = Head
  { _head_name :: HeadName
  -- ^ Unique name of the Head
  , _head_participants :: Set Address
      --- Map Address (Address, HydraKeyInfo)
  , _head_status :: Status
  , _head_status_bchan :: TChan Status
  -- ^ The participants list with proxy addresses and not owner addresses
  }

-- | A Hydra Node running as part of a network
data Node = Node
  { _node_handle :: ProcessHandle
  , _node_info :: HydraNodeInfo
  , _node_monitor_thread :: ThreadId
  , _node_send_msg :: ClientInput -> IO ()
  , _node_get_listen_chan :: IO (TChan (ServerOutput Value))
  }

-- | The network of nodes that hold up a head
newtype Network = Network
  { _network_nodes :: Map Address Node
  }

getHydraPayState :: (MonadIO m)
  => HydraSharedInfo
  -> m State
getHydraPayState hydraSharedInfo = do
  addrs <- liftIO $ newMVar mempty
  heads <- liftIO $ newMVar mempty
  networks <- liftIO $ newMVar mempty
  path <- liftIO $ getKeyPath
  pure $ State hydraSharedInfo addrs heads networks path

data Tx = Tx
  { txType :: T.Text
  , txDescription :: T.Text
  , txCborHex :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tx where
  toJSON (Tx t d c) =
    object [ "type" .= t
           , "description" .= d
           , "cborHex" .= c
           ]

instance FromJSON Tx where
  parseJSON = withObject "Tx" $ \v -> Tx
    <$> v .: "type"
    <*> v .: "description"
    <*> v .: "cborHex"

data TxType =
  Funds | Fuel
  deriving (Eq, Show, Generic)

instance ToJSON TxType
instance FromJSON TxType

isFuelType :: TxType -> Bool
isFuelType Fuel = True
isFuelType _ = False


headBalance :: (MonadIO m) => State -> HeadName -> Address -> m (Either HydraPayError Lovelace)
headBalance state name addr = do
  withNode state name addr $ \node proxyAddr -> do
    utxos <- getNodeUtxos node proxyAddr
    let
      txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
      fullAmount = sum txInAmounts
    pure (Right fullAmount)


stateCardanoNodeInfo :: State -> CardanoNodeInfo
stateCardanoNodeInfo = _cardanoNodeInfo . _state_hydraInfo

l1Balance :: (MonadIO m) => State -> Address -> Bool -> m Lovelace
l1Balance state addr includeFuel = do
  utxos <- queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ (bool filterOutFuel id includeFuel) utxos
    fullAmount = sum txInAmounts
  pure fullAmount

-- | The balance of the proxy address associated with the address given
getProxyFunds :: (MonadIO m) => State -> Address -> m Lovelace
getProxyFunds state addr = do
  (proxyAddr, _) <- addOrGetKeyInfo state addr
  l1Balance state proxyAddr False

getProxyFuel :: (MonadIO m) => State -> Address -> m Lovelace
getProxyFuel  state addr = do
  (proxyAddr, _) <- addOrGetKeyInfo state addr
  utxos <- queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ filterFuel utxos
    fullAmount = sum txInAmounts
  pure fullAmount

buildAddTx :: MonadIO m => TxType -> State -> Address -> Lovelace -> m (Either HydraPayError Tx)
buildAddTx txType state fromAddr amount = do
  utxos <- queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) fromAddr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
  (toAddr, _) <- addOrGetKeyInfo state fromAddr

  let
    fullAmount = sum txInAmounts
    txInCount = Map.size txInAmounts
  txBodyPath <- liftIO $ snd <$> getTempPath
  _ <- liftIO $ readCreateProcess (proc cardanoCliPath
                       (filter (/= "") $ [ "transaction"
                        , "build"
                        , "--babbage-era"
                        , "--cardano-mode"
                        ]
                        <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) . Map.keys $ txInAmounts)
                        <>
                        [ "--tx-out"
                        , [i|#{toAddr}+#{amount}|]
                        ]
                        <> bool [] [ "--tx-out-datum-hash", T.unpack fuelMarkerDatumHash ] (isFuelType txType)
                        <>
                        [ "--change-address"
                        , T.unpack fromAddr
                        , "--testnet-magic"
                        , show . _testNetMagic . _nodeType $ _cardanoNodeInfo . _state_hydraInfo $ state
                        ]
                        <>
                        [ "--out-file"
                        , txBodyPath
                        ])) { env = Just [( "CARDANO_NODE_SOCKET_PATH"
                                          , _nodeSocket . _cardanoNodeInfo . _state_hydraInfo $ state)] }
    ""
  txResult <- liftIO $ readFile txBodyPath
  case Aeson.decode $ LBS.pack txResult of
    Just tx -> pure $ Right tx
    Nothing -> pure $ Left FailedToBuildFundsTx

-- TODO: HydraPayError is too flat
-- TODO: Add a reader data type for State
withNetwork ::
  MonadIO m =>
  State ->
  HeadName ->
  (Network -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withNetwork state name f = maybe (pure $ Left NetworkIsn'tRunning) f =<< getNetwork state name

withNode ::
  MonadIO m =>
  State ->
  HeadName ->
  Address ->
  (Node -> Address -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withNode state name addr f = do
  withNetwork state name $ \network -> do
      (proxyAddr, _) <- addOrGetKeyInfo state addr
      case Map.lookup proxyAddr $ _network_nodes network of
        Nothing -> pure $ Left NotAParticipant
        Just node -> f node proxyAddr

initHead :: MonadIO m => State -> HeadInit -> m (Either HydraPayError ())
initHead state (HeadInit name _ con) = do
  (fmap.fmap) (const ()) $ sendToHeadAndWaitFor (Init $ fromIntegral con) (\case
    ReadyToCommit {} -> True
    _ -> False) state name

data WithdrawRequest = WithdrawRequest
  { withdraw_address :: Address
  -- | Nothing to redraw all.
  , withdraw_amount :: Maybe Lovelace
  }
  deriving(Eq, Show, Generic)

instance ToJSON WithdrawRequest
instance FromJSON WithdrawRequest


-- | Withdraw funds (if available from proxy request), fee is subtracted.
withdraw :: MonadIO m => State -> WithdrawRequest -> m (Either HydraPayError TxId)
withdraw state (WithdrawRequest addr maybeAmount) = do
  (proxyAddr, keyInfo) <- addOrGetKeyInfo state addr
  utxos <- queryAddressUTXOs nodeInfo proxyAddr
  let
    notFuel = filterOutFuel utxos
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) notFuel
    total = sum txInAmounts

  liftIO $ putStrLn $ "Address has: " <> show total <> " Lovelace"
  let signingKey = _signingKey $ _cardanoKeys keyInfo
  result <- liftIO $ transferAmount nodeInfo signingKey txInAmounts addr True maybeAmount
  pure $ maybe (Left InsufficientFunds) Right result
  where
    nodeInfo = _cardanoNodeInfo . _state_hydraInfo $ state

fanoutHead :: MonadIO m => State -> HeadName -> m (Either HydraPayError HeadStatus)
fanoutHead = sendToHeadAndWaitFor Fanout $ \case
  HeadIsFinalized {} -> True
  _ -> False

closeHead :: MonadIO m => State -> HeadName -> m (Either HydraPayError HeadStatus)
closeHead = sendToHeadAndWaitFor Close $ \case
  HeadIsClosed {} -> True
  _ -> False

sendToHeadAndWaitFor :: MonadIO m => ClientInput -> (ServerOutput Value -> Bool) -> State -> HeadName -> m (Either HydraPayError HeadStatus)
sendToHeadAndWaitFor ci fso state headName = do
  mNetwork <- getNetwork state headName
  case mNetwork of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just network -> do
      -- TODO: Round robin to prevent exhausting gas of the first participant!
      let firstNode = head $ Map.elems $ _network_nodes network
          sendMsg = _node_send_msg firstNode
          getChan = _node_get_listen_chan firstNode
      liftIO $ do
        channel <- getChan
        sendMsg ci
        let
          waitForCloseHandler = do
            output <- atomically $ readTChan channel
            case (fso output) of
              True  -> pure ()
              _ -> waitForCloseHandler
        waitForCloseHandler
      statusResult <- getHeadStatus state headName
      pure $ statusResult

commitToHead :: MonadIO m => State -> HeadCommit -> m (Either HydraPayError ())
commitToHead state (HeadCommit name addr amount) = do
  withNode state name addr $ \node proxyAddr -> do

    let
      rightAmount (TxInInfo _ _ val) =
        Map.lookup "lovelace" val == Just amount

    -- Get the first UTXO that has the correct amount as we MUST commit 1 or no UTXOs
    proxyFunds <- Map.take 1 . Map.filter rightAmount . filterOutFuel <$> queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) proxyAddr

    case Map.null proxyFunds of
      True -> pure $ Left NoValidUTXOToCommit
      False -> do
        sendToNodeAndListen node (Commit proxyFunds) $ (\case
          Committed {} -> Just $ Right ()
          CommandFailed {} -> Just $ Left NodeCommandFailed
          _ -> Nothing)
        where
          sendToNodeAndListen node ci fso = do
            let
              sendMsg = _node_send_msg node
              getChan = _node_get_listen_chan node

            liftIO $ do
              channel <- getChan
              sendMsg ci
              let
                waitForCloseHandler = do
                  output <- atomically $ readTChan channel
                  case (fso output) of
                    Just x  -> pure x
                    _ -> waitForCloseHandler
              waitForCloseHandler



-- | Create a head by starting a Hydra network.
createHead :: MonadIO m => State -> HeadCreate -> m (Either HydraPayError Head)
createHead state (HeadCreate name participants) = runExceptT $ do
  when (null participants) $ throwError NotEnoughParticipants
  -- FIXME(parenthetical): I think there are concurrency issues here?
  -- What if createHead is called twice quickly?
  headExists <- isJust <$> lookupHead state name
  when headExists $ throwError $ HeadExists name
  statusBTChan <- liftIO newBroadcastTChanIO
  let head = Head name (Set.fromList participants) Status_Pending statusBTChan
  liftIO $ modifyMVar_ (_state_heads state) $ pure . Map.insert name head
  startNetwork state head
  pure head

data HydraTxError =
  HydraTxNotSeen

filterUtxos :: Address -> WholeUTXO -> WholeUTXO
filterUtxos addr = Map.filter ((== addr) . HT.address)


getNodeUtxos :: MonadIO m => Node -> Address -> m (Map TxIn TxInInfo)
getNodeUtxos node proxyAddr = do
  liftIO $ _node_send_msg node $ GetUTxO
  c <- liftIO $ _node_get_listen_chan node
  untilJust $ do
    x <- liftIO . atomically $ readTChan c
    case x of
      GetUTxOResponse utxoz -> do
        pure $ Just (filterUtxos proxyAddr utxoz)
      _ -> pure Nothing


submitTxOnHead :: (MonadIO m) => State -> Address -> HeadSubmitTx -> m (Either HydraPayError Pico)
submitTxOnHead state addr (HeadSubmitTx name toAddr amount) = do
  withNode state name addr $ \node proxyAddr -> do
    c <- liftIO $ _node_get_listen_chan node
    senderUtxos :: Map TxIn TxInInfo <- getNodeUtxos node proxyAddr
    let signingKey = _signingKey . _cardanoKeys . _keys . _node_info $ node
    let lovelaceUtxos = Map.mapMaybe (Map.lookup "lovelace" . HT.value) senderUtxos
    -- FIXME: fails on empty lovelaceUtxos
    (toAddrProxy, _) <- addOrGetKeyInfo state toAddr
    signedTxJsonStr <- liftIO $ buildSignedHydraTx signingKey proxyAddr toAddrProxy lovelaceUtxos amount
    let jsonTx :: Aeson.Value = fromMaybe (error "Failed to parse TX") . Aeson.decode . LBS.pack $ signedTxJsonStr
    let txCborHexStr = fromJust . parseMaybe (withObject "signed tx" (.: "cborHex")) $ jsonTx

    startTime <- liftIO $ getCurrentTime
    liftIO $ _node_send_msg node $ NewTx . T.pack $ txCborHexStr
    -- TODO: Could we make sure we saw the transaction that we sent and not another one?
    untilJust $ do
      x <- liftIO . atomically $ readTChan c
      case x of
        TxValid tx -> do
          endTime <- liftIO $ getCurrentTime
          pure . Just $ Right (nominalDiffTimeToSeconds $ diffUTCTime endTime startTime)
        ServerOutput.TxInvalid utxo tx validationError ->
          pure . Just $ (Left (HydraPay.Api.TxInvalid utxo tx validationError))
        _ -> pure Nothing

removeHead :: (MonadIO m) => State -> HeadName -> m ()
removeHead state name = do
  terminateHead state name
  liftIO $ modifyMVar_ (_state_heads state) $ pure . Map.delete name

terminateHead :: (MonadIO m) => State -> HeadName -> m ()
terminateHead state headName = do
  liftIO $ modifyMVar_ (_state_networks state) $ \ns -> do
    -- TODO: warn about nonexistence?
    flip (maybe (pure ns)) (Map.lookup headName ns) $ \n -> do
      mapM_ (terminateProcess . _node_handle) . _network_nodes $ n
      pure (Map.delete headName ns)

-- | Lookup head via name
lookupHead :: MonadIO m => State -> HeadName -> m (Maybe Head)
lookupHead state name = do
  liftIO $ withMVar (_state_heads state) (pure . Map.lookup name)

-- NOTE(skylar): Idempotent
-- | Generate a proxy address with keys,
addOrGetKeyInfo :: MonadIO m => State -> Address -> m (Address, HydraKeyInfo)
addOrGetKeyInfo state addr = do
  liftIO $ modifyMVar (_state_proxyAddresses state) $ \old -> do
    case Map.lookup addr old of
      Just info -> pure (old, info)
      Nothing -> do
        keyInfo <- withLogging $ generateKeysIn $ T.unpack addr <> "-proxy"
        proxyAddress <- liftIO
                        $ getCardanoAddress (_cardanoNodeInfo . _state_hydraInfo $ state)
                        $ _verificationKey . _cardanoKeys $ keyInfo
        let info = (proxyAddress, keyInfo)
        pure $ (Map.insert addr info old, info)
  where
    path = _state_keyPath state

getHeadStatus :: MonadIO m => State -> HeadName -> m (Either HydraPayError HeadStatus)
getHeadStatus state name = liftIO $ do
  mHead <- lookupHead state name
  case mHead of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just (Head name _ status _) -> do
      running <- isJust <$> getNetwork state name
      pure $ Right $ HeadStatus name running status


-- | Wait for a head to change to or past a state. Returns 'Nothing'
-- if the head was not found, otherwise 'Just SomeStatus'.
waitForHeadStatus :: (MonadIO m) => State -> HeadName -> Status -> m (Maybe Status)
waitForHeadStatus state name status = runMaybeT $ do
  hd <- MaybeT $ lookupHead state name
  c <- liftIO . atomically $ dupTChan $ _head_status_bchan hd
  hd <- MaybeT $ lookupHead state name
  if _head_status hd >= status
    then pure $ _head_status hd
    else untilJust $ do
      x <- liftIO . atomically $ readTChan c
      if x >= status
        then pure $ Just x
        else pure $ Nothing

-- | Start a network for a given Head, trying to start a network that already exists is a no-op and you will just get the existing network
startNetwork :: MonadIO m => State -> Head -> m Network
startNetwork state (Head name participants _ statusBTChan) = do
  mNetwork <- getNetwork state name
  case mNetwork of
    Just network -> pure network
    Nothing -> do
      proxyMap <- participantsToProxyMap state participants
      nodes <- startHydraNetwork (_state_hydraInfo state) proxyMap

      liftIO $ putStrLn $ intercalate "\n" . fmap (show . _port . snd) . Map.elems $ nodes
      let numNodes = length nodes
      readyCounterChan <- liftIO $ newChan
      network <- fmap Network . forM nodes $ \(processHndl, nodeInfo) -> do
        let port =  _apiPort $ nodeInfo
        bRcvChan <- liftIO newBroadcastTChanIO
        sndChan <- liftIO newChan
        monitor <- liftIO $ forkIO $ do
          -- FIXME: Instead of threadDelay retry connecting to the WS port
          threadDelay 3000000
          putStrLn $ [i|Connecting to WS port #{port}|]
          runClient "127.0.0.1" port "/" $ \conn -> do
            putStrLn [i|Connected to node on port #{port}|]
            -- Fork off "message send" thread
            void $ forkIO $ forever $ do
              toSnd :: ClientInput <- liftIO $ readChan sndChan
              putStrLn $ [i|Sending to WS port #{port}: #{show toSnd}|]
              WS.sendTextData conn $ Aeson.encode toSnd
            let getParsedMsg = do
                msgLBS :: LBS.ByteString <- WS.receiveData conn
                let msg = fromMaybe (error $ "FIXME: Cardanode Node message we could not parse!?\n"
                                    <> LBS.unpack msgLBS)
                          $ decode' msgLBS
                putStrLn $ "Monitor:" <> show port <> ": " <> show msg
                pure msg
            -- Wait until we have seen all other nodes connect to this one
            flip (iterateUntilM (== (numNodes - 1))) 0 $ \n -> do
              msg <- getParsedMsg
              -- TODO: Maybe best to actually check which peer rather
              -- than blindly count?
              pure $ case msg of
                PeerConnected _ -> n + 1
                _ -> n
            writeChan readyCounterChan ()
            -- Start accepting messages
            forever $ do
              msg <- getParsedMsg
              let handleMsg = \case
                    ReadyToCommit {} -> Just Status_Init
                    Committed {} -> Just Status_Committing
                    HeadIsOpen {} -> Just Status_Open
                    HeadIsClosed _ _ -> Just Status_Closed
                    ReadyToFanout {} -> Just Status_Fanout
                    HeadIsAborted {} -> Nothing
                    HeadIsFinalized {} -> Just Status_Finalized
                    _ -> Nothing
              case handleMsg msg of
                Just status -> do
                  liftIO $ modifyMVar_ (_state_heads state) $ \current -> do
                    case _head_status <$> Map.lookup name current of
                      Just currentStatus -> do
                        when (currentStatus /= status && status == Status_Fanout) $ void $ forkIO $ do
                         _ <- fanoutHead state name
                         pure ()
                      _ -> pure ()
                    atomically $ writeTChan statusBTChan status
                    pure . Map.adjust (\h -> h { _head_status = status }) name $ current
                Nothing -> pure ()
              atomically $ writeTChan bRcvChan msg
        pure $ Node
          { _node_handle = processHndl
          , _node_info = nodeInfo
          , _node_monitor_thread = monitor
          , _node_send_msg = writeChan sndChan
          , _node_get_listen_chan = atomically $ dupTChan bRcvChan
          }
      liftIO $ putStrLn [i|---- Waiting for #{numNodes} nodes to get ready |]
      let countReadies n = do
            if n == numNodes
              then putStrLn [i|---- All nodes ready|]
              else do
                readChan readyCounterChan
                putStrLn [i|---- #{n + 1} nodes ready of #{numNodes}|]
                countReadies (n + 1)
      liftIO $ countReadies 0

      -- Add the network to the running networks mvar
      liftIO $ modifyMVar_ (_state_networks state) $ pure . Map.insert name network
      pure network

-- | This takes the set participants in a Head and gets their proxy equivalents as actual addresses
-- participating in the network are not the addresses registered in the head, but their proxies
participantsToProxyMap :: MonadIO m => State -> Set Address -> m (Map Address HydraKeyInfo)
participantsToProxyMap state participants =
  liftIO $ fmap Map.fromList $ for (Set.toList participants) $ addOrGetKeyInfo state

-- | Lookup the network associated with a head name
getNetwork :: MonadIO m => State -> HeadName -> m (Maybe Network)
getNetwork state name =
  liftIO $ withMVar (_state_networks state) (pure . Map.lookup name)

startHydraNetwork :: (MonadIO m)
  => HydraSharedInfo
  -> Map Address HydraKeyInfo
  -> m (Map Address (ProcessHandle, HydraNodeInfo))
startHydraNetwork sharedInfo actors = do
  liftIO $ createDirectoryIfMissing True "demo-logs"
  liftIO $ sequence . flip Map.mapWithKey nodes $ \name node -> do
    logHndl <- openFile [iii|demo-logs/hydra-node-#{name}.log|] WriteMode
    errHndl <- openFile [iii|demo-logs/hydra-node-#{name}.error.log|] WriteMode
    let cp = (mkHydraNodeCP sharedInfo node (filter ((/= _nodeId node) . _nodeId) (Map.elems nodes)))
             { std_out = UseHandle logHndl
             , std_err = UseHandle errHndl
             }
    (_,_,_,handle) <- createProcess cp
    pure (handle, node)
  where
    portNum p n = p * 1000 + n
    node (n, (name, keys)) =
      ( name
      , HydraNodeInfo n (portNum 5 n) (portNum 9 n) (portNum 6 n) keys
      )
    nodes = Map.fromList . fmap node $ zip [1 ..] (Map.toList actors)

data HydraSharedInfo = HydraSharedInfo
  { _hydraScriptsTxId :: String
  , _cardanoNodeInfo :: CardanoNodeInfo
  }

data HydraNodeInfo = HydraNodeInfo
  { _nodeId :: Int
  , _port :: Int
  -- ^ The port this node is running on
  , _apiPort :: Int
  -- ^ The port that this node is serving its pub/sub websockets api on
  , _monitoringPort :: Int
  , _keys :: HydraKeyInfo
  }

-- | Takes the node participant and the list of peers
mkHydraNodeCP :: HydraSharedInfo -> HydraNodeInfo -> [HydraNodeInfo] -> CreateProcess
mkHydraNodeCP sharedInfo node peers =
  (proc hydraNodePath $ sharedArgs sharedInfo <> nodeArgs node <> concatMap peerArgs peers)
  { std_out = Inherit
  }

cardanoNodeArgs :: CardanoNodeInfo -> [String]
cardanoNodeArgs cninf =
  ["--network-id"
  , show . _testNetMagic . _nodeType $ cninf
  , "--node-socket"
  , _nodeSocket cninf
  ]

sharedArgs :: HydraSharedInfo -> [String]
sharedArgs (HydraSharedInfo hydraScriptsTxId cardanoNodeInfo) =
  [ "--ledger-genesis"
  , _nodeLedgerGenesis cardanoNodeInfo
  , "--ledger-protocol-parameters"
  , _nodeLedgerProtocolParameters cardanoNodeInfo
  , "--hydra-scripts-tx-id"
  , hydraScriptsTxId
  ] <> cardanoNodeArgs cardanoNodeInfo

nodeArgs :: HydraNodeInfo -> [String]
nodeArgs (HydraNodeInfo nodeId port apiPort monitoringPort
           (HydraKeyInfo
            (KeyPair cskPath _cvkPath)
            (KeyPair hskPath _hvkPath))) =
  [ "--node-id"
  , show nodeId
  , "--port"
  , show port
  , "--api-port"
  , show apiPort
  , "--monitoring-port"
  , show monitoringPort
  , "--hydra-signing-key"
  , getSigningKeyFilePath hskPath
  , "--cardano-signing-key"
  , getSigningKeyFilePath cskPath
  ]

peerArgs :: HydraNodeInfo -> [String]
peerArgs ni =
  [ "--peer"
  , [i|127.0.0.1:#{_port ni}|]
  , "--hydra-verification-key"
  , getVerificationKeyFilePath $ _verificationKey . _hydraKeys . _keys $ ni
  , "--cardano-verification-key"
  , getVerificationKeyFilePath $ _verificationKey . _cardanoKeys . _keys $ ni
  ]

cardanoNodeCreateProcess :: CreateProcess
cardanoNodeCreateProcess =
  (proc cardanoNodePath
   [ "run"
   , "--config"
   , "devnet/cardano-node.json"
   , "--topology"
   , "devnet/topology.json"
   , "--database-path"
   , "devnet/db"
   , "--socket-path"
   , "devnet/node.socket"
   , "--shelley-operational-certificate"
   , "devnet/opcert.cert"
   , "--shelley-kes-key"
   , "devnet/kes.skey"
   , "--shelley-vrf-key"
   , "devnet/vrf.skey"
   ]) { std_out = CreatePipe
      }
