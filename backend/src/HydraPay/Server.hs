-- |
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HydraPay.Server where


import Snap.Core
import Data.CaseInsensitive
import Network.WebSockets.Snap
import qualified Network.WebSockets as WS

import Prelude hiding ((.))
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Control.Category ((.))
import System.Process
import GHC.Generics
import qualified Data.ByteString as BS
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
import System.IO (IOMode(WriteMode), openFile, Handle)
import Network.WebSockets.Client
import qualified Network.WebSockets.Connection as WS

import Crypto.Random
import Data.ByteString.Base64 as Base64

import Control.Monad.Reader
import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Control.Applicative

import qualified Network.WebSockets as WS

import Common.Helpers

import Control.Concurrent
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.Compat (nominalDiffTimeToSeconds)
import Data.Fixed

import Data.Foldable

import Data.Text.Prettyprint.Doc
import Control.Monad.Log
import System.Directory

import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as ConfigIO

import HydraPay.Api

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

import Text.Read (readMaybe)
import Prelude hiding ((.))
import Control.Lens hiding ((.=))
import Control.Lens.TH
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
import System.IO (IOMode(WriteMode), openFile, hClose, Handle)
import Network.WebSockets.Client
import qualified Network.WebSockets.Connection as WS

import Common.Helpers

import Control.Concurrent
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.Compat (nominalDiffTimeToSeconds)
import Data.Fixed

import Data.Foldable

import Data.Text.Prettyprint.Doc
import Control.Monad.Log
import System.Directory

import HydraPay.Api

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
import System.IO.Temp (withTempFile)

{-
Architecture!

HydraPay must manage all hydra nodes and heads

WebSocket client!
We need websockets for everything, because we need request responses and notification style stuff to write dApps and such
The client facing dapp sockets need to be able to notify us about whatever is going on.

Creating a head should probably just init right away, but give us a chance to change things and do whatever...
A lot of the state management is just waiting for things to happen, with some triggers here and there..

The devnet must be completely separate

The nodes provide and respond to messages

Hydra pay tracks their state and provides and responds to messages.
-}


-- | The location where we store cardano and hydra keys
getKeyPath :: IO FilePath
getKeyPath = do
  createDirectoryIfMissing True path
  pure path
  where
    path = "keys"

withLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
withLogging = flip runLoggingT (print . renderWithSeverity id)


getDevnetHydraSharedInfo :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraSharedInfo
getDevnetHydraSharedInfo = do
  scripts <- getReferenceScripts "devnet/scripts" (_signingKey devnetFaucetKeys)
  pure $ HydraSharedInfo
    { _hydraScriptsTxId = T.unpack scripts,
      _hydraLedgerGenesis = "devnet/genesis-shelley.json",
      _hydraLedgerProtocolParameters = "devnet/protocol-parameters.json",
      _hydraCardanoNodeInfo = cardanoDevnetNodeInfo
    }

-- | State we need to run/manage Heads
data State = State
  { _state_cardanoNodeState :: MVar CardanoNodeState
  , _state_proxyAddresses :: MVar (Map Address (Address, HydraKeyInfo))
  -- ^ This is really temporary it has the mapping from cardano address to proxy + keys
  , _state_heads :: MVar (Map HeadName Head)
  , _state_subscribers :: MVar (Map HeadName [WS.Connection])
  -- ^ This is really temporary until we stuff it all in a database
  , _state_networks :: MVar (Map HeadName Network)
  -- , _state_connectionPool :: Pool Connection -- We could ignore htis for now
  , _state_keyPath :: FilePath

  , _state_getPorts :: Int -> IO (Maybe [Int])
  , _state_freePorts :: [Int] -> IO ()
  , _state_apiKey :: BS.ByteString
  }

-- The Process of the node and the HydraSharedInfo for that node
data CardanoNodeState = CardanoNodeState
  { cardanoNodeState_processHandles :: Maybe ProcessHandles
  , cardanoNodeState_hydraSharedInfo :: HydraSharedInfo
  , cardanoNodeState_nodeType :: NodeConfig
  }

type ProcessHandles = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | A Hydra Head in Hydra Pay
data Head = Head
  { _head_name :: HeadName
  -- ^ Unique name of the Head
  , _head_participants :: Set Address
  -- ^ The participants list with proxy addresses and not owner addresses
  , _head_status :: Status
  , _head_status_bchan :: TChan Status
  -- ^ The ports used by the nodes in this head
  }

-- | A Hydra Node running as part of a network
data Node = Node
  { _node_handle :: ProcessHandle
  , _node_info :: HydraNodeInfo
  , _node_monitor_thread :: ThreadId
  , _node_send_msg :: ClientInput -> IO ()
  , _node_get_listen_chan :: IO (TChan (ServerOutput Value))
  , _node_ports :: [Int]
  }

-- | The network of nodes that hold up a head
newtype Network = Network
  { _network_nodes :: Map Address Node
  }

runHydraPay :: (State -> IO a)
  -> IO a
runHydraPay action = do
  configs <- ConfigIO.getConfigs
  result <- runConfigsT configs $ runMaybeT $ do
    MaybeT $ getConfig "backend/api-key"

  key <- case result of
    Nothing -> do
      putStrLn "Generating New API Key..."
      newKey <- Base64.encode <$> getRandomBytes 32
      BS.writeFile "config/backend/api-key" newKey
      pure newKey
    Just key -> pure key

  addrs <- liftIO $ newMVar mempty
  subs <- liftIO $ newMVar mempty

  heads <- liftIO $ newMVar mempty
  networks <- liftIO $ newMVar mempty
  path <- liftIO $ getKeyPath
  ports <- liftIO $ newMVar ([9000..11000] :: [Int])
  let getPorts n = modifyMVar ports $ \ps -> pure $
        let (xs,ys) = splitAt n ps
        in if null ys
           then (ps, Nothing)
           else (ys, Just xs)
  let freePorts ps = void $ modifyMVar_ ports (pure . (ps ++))
  cardanoNodeState <- withLogging getCardanoNodeState >>= newMVar
  action $ State cardanoNodeState addrs heads networks subs path getPorts freePorts key

getHydraSharedInfo :: MonadIO m => State -> m HydraSharedInfo
getHydraSharedInfo = liftIO .
  fmap cardanoNodeState_hydraSharedInfo . readMVar . _state_cardanoNodeState

stateCardanoNodeInfo :: MonadIO m => State -> m CardanoNodeInfo
stateCardanoNodeInfo = fmap _hydraCardanoNodeInfo . getHydraSharedInfo

getCardanoNodeState :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m CardanoNodeState
getCardanoNodeState = do
  cfg <- liftIO readNodeConfig
  case cfg of
    CfgDevnet -> liftIO $ do
      readCreateProcess (shell "rm -rf devnet") ""
      readCreateProcess (shell "rm -rf demo-logs") ""
      handles <- createProcess cardanoNodeCreateProcess
      _ <- liftIO $ readCreateProcess ((proc cardanoCliPath [ "query"
                                        , "protocol-parameters"
                                        , "--testnet-magic"
                                        , "42"
                                        , "--out-file"
                                        , "devnet/devnet-protocol-parameters.json"
                                        ])
                { env = Just [( "CARDANO_NODE_SOCKET_PATH" , "devnet/node.socket")]
                }) ""
      hydraSharedInfo <- withLogging getDevnetHydraSharedInfo
      pure $ CardanoNodeState (Just handles) hydraSharedInfo CfgDevnet
      {-
      liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ _stdout _ _handle -> do
        flip runLoggingT (print . renderWithSeverity id) $ do
          liftIO $ threadDelay (seconds 3)
          logMessage $ WithSeverity Informational [i|
            Cardano node is running
            |]
          _ <- liftIO $ readCreateProcess ((proc cardanoCliPath [ "query"
                                        , "protocol-parameters"
                                        , "--testnet-magic"
                                        , "42"
                                        , "--out-file"
                                        , "devnet/devnet-protocol-parameters.json"
                                        ])
                { env = Just [( "CARDANO_NODE_SOCKET_PATH" , "devnet/node.socket")]
                }) ""
          prefix <- liftIO getTempPath'
          oneKs <- generateCardanoKeys $ prefix <> "one"
          one <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey oneKs
          twoKs <- generateCardanoKeys $ prefix <> "two"
          two <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey twoKs
          seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys one (ada 10000) False
          seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys two (ada 10000) False
          state <- getHydraPayState =<< getDevnetHydraSharedInfo
          f state cardanoDevnetNodeInfo [(oneKs, one), (twoKs, two)]-}
    CfgPreview pcfg -> liftIO $ --liftIO . flip runLoggingT (print . renderWithSeverity id) $ do
      pure $ CardanoNodeState Nothing (_previewHydraInfo pcfg) (CfgPreview pcfg)
      {-
      state <- getHydraPayState (_previewHydraInfo pcfg)
      participants <- forM (_previewParticipants pcfg) $ \kp -> do
        addr <- liftIO $ getCardanoAddress (_previewNodeInfo pcfg) (_verificationKey kp)
        pure (kp, addr)
      f state (_previewNodeInfo pcfg) participants-}

data NodeConfig
  = CfgDevnet
  | CfgPreview PreviewNodeConfig
  deriving (Show,Read)

data PreviewNodeConfig = PreviewNodeConfig
  { _previewNodeInfo :: CardanoNodeInfo
  , _previewHydraInfo :: HydraSharedInfo
  , _previewFaucet :: KeyPair
  , _previewParticipants :: [KeyPair]
  }
  deriving (Show,Read)

demoCfgPath :: FilePath
demoCfgPath = "democonfig"

writeNodeConfig :: NodeConfig -> IO ()
writeNodeConfig cfg = do
  writeFile demoCfgPath . show $ cfg

readNodeConfig :: IO NodeConfig
readNodeConfig = fmap (fromMaybe CfgDevnet) . runMaybeT $ do
  guard <=< liftIO $ doesFileExist demoCfgPath
  MaybeT $ readMaybe <$> readFile demoCfgPath

seedDemoAddressPreview :: PreviewNodeConfig -> Lovelace -> KeyPair -> IO TxIn
seedDemoAddressPreview cfg amount kp = flip runLoggingT (print . renderWithSeverity id) $ do
    addr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey kp)
    seedAddressFromFaucetAndWait (_previewNodeInfo cfg) (_previewFaucet $ cfg) addr amount False

seedDemoAddressesPreview :: PreviewNodeConfig -> Lovelace -> IO ()
seedDemoAddressesPreview cfg amount =
  forM_ (_previewParticipants cfg) $ seedDemoAddressPreview cfg amount

deseedDemoAddressesPreview :: PreviewNodeConfig -> IO ()
deseedDemoAddressesPreview cfg =
  forM_ (_previewParticipants cfg) $ \kp -> do
    addr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey kp)
    faucetAddr <- liftIO $ getCardanoAddress (_previewNodeInfo cfg) (_verificationKey (_previewFaucet cfg))
    transferAll (_previewNodeInfo cfg) (_signingKey kp) addr faucetAddr


whenDevnet :: (Applicative m) => NodeConfig -> m () -> m ()
whenDevnet cfg = when (case cfg of
                          CfgDevnet -> True
                          _ -> False)

teardownDevnet :: CardanoNodeState -> IO ()
teardownDevnet (CardanoNodeState handles sharedinfo _) = do
  maybe (pure ()) cleanupProcess handles
  exists <- doesPathExist "devnet"
  when exists $ removePathForcibly "devnet"

headBalance :: (MonadIO m) => State -> HeadName -> Address -> m (Either HydraPayError Lovelace)
headBalance state name addr = do
  withNode state name addr $ \node proxyAddr -> do
    utxos <- getNodeUtxos node proxyAddr
    let
      txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
      fullAmount = sum txInAmounts
    pure (Right fullAmount)

l1Balance :: (MonadIO m) => State -> Address -> Bool -> m Lovelace
l1Balance state addr includeFuel = do
  hydraInfo <- getHydraSharedInfo state
  utxos <- queryAddressUTXOs (_hydraCardanoNodeInfo hydraInfo) addr
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
getProxyFuel state addr = do
  (proxyAddr, _) <- addOrGetKeyInfo state addr
  hydraInfo <- getHydraSharedInfo state
  utxos <- queryAddressUTXOs (_hydraCardanoNodeInfo hydraInfo) addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ filterFuel utxos
    fullAmount = sum txInAmounts
  pure fullAmount

buildAddTx :: MonadIO m => TxType -> State -> Address -> Lovelace -> m (Either HydraPayError Tx)
buildAddTx txType state fromAddr amount = do
  cardanoNodeInfo <- _hydraCardanoNodeInfo <$> getHydraSharedInfo state
  utxos <- queryAddressUTXOs cardanoNodeInfo fromAddr
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
                        , show . _testNetMagic . _nodeType $ cardanoNodeInfo
                        ]
                        <>
                        [ "--out-file"
                        , txBodyPath
                        ])) { env = Just [( "CARDANO_NODE_SOCKET_PATH"
                                          , _nodeSocket cardanoNodeInfo)] }
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

withAnyNode ::
  MonadIO m =>
  State ->
  HeadName ->
  (Node -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withAnyNode state name f = do
  withNetwork state name $ \network -> do
    case headMay $ Map.elems (_network_nodes network) of
      Nothing -> pure $ Left NotAParticipant
      Just node -> f node

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
  hydraInfo <- getHydraSharedInfo state
  let
    nodeInfo = _hydraCardanoNodeInfo hydraInfo
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

    cardanoNodeInfo <- _hydraCardanoNodeInfo <$> getHydraSharedInfo state
    -- Get the first UTXO that has the correct amount as we MUST commit 1 or no UTXOs
    proxyFunds <- Map.take 1 . Map.filter rightAmount . filterOutFuel <$> queryAddressUTXOs cardanoNodeInfo proxyAddr

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
      _state_freePorts state . concatMap _node_ports . Map.elems . _network_nodes $ n
      pure (Map.delete headName ns)

-- | Lookup head via name
lookupHead :: MonadIO m => State -> HeadName -> m (Maybe Head)
lookupHead state name = do
  liftIO $ withMVar (_state_heads state) (pure . Map.lookup name)

-- NOTE(skylar): Idempotent
-- | Generate a proxy address with keys,
addOrGetKeyInfo :: MonadIO m => State -> Address -> m (Address, HydraKeyInfo)
addOrGetKeyInfo state addr = do
  cardanoNodeInfo <- _hydraCardanoNodeInfo <$> getHydraSharedInfo state
  liftIO $ modifyMVar (_state_proxyAddresses state) $ \old -> do
    case Map.lookup addr old of
      Just info -> pure (old, info)
      Nothing -> do
        keyInfo <- withLogging $ generateKeysIn $ T.unpack addr <> "-proxy"
        proxyAddress <- liftIO
                        $ getCardanoAddress cardanoNodeInfo
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
      hydraInfo <- getHydraSharedInfo state
      nodes <- startHydraNetwork hydraInfo proxyMap (_state_getPorts state)
      let numNodes = length nodes
      readyCounterChan <- liftIO $ newChan
      network <- fmap Network . forM nodes $ \(processHndl, nodeInfo, ports) -> do
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
                    HeadIsClosed {} -> Just Status_Closed
                    ReadyToFanout {} -> Just Status_Fanout
                    HeadIsAborted {} -> Nothing
                    HeadIsFinalized {} -> Just Status_Finalized
                    _ -> Nothing
              case handleMsg msg of
                Just status -> do
                  liftIO $ modifyMVar_ (_state_heads state) $ \current -> do
                    case _head_status <$> Map.lookup name current of
                      Just currentStatus -> do
                        -- When the status is Fanout we should issue the fanout
                        -- Do we actually need to wait for the fanout or should it just be fine without it?
                        when (currentStatus /= status && status == Status_Fanout) $ void $ forkIO $ do
                         _ <- fanoutHead state name
                         pure ()
                      _ -> pure ()
                    atomically $ writeTChan statusBTChan status
                    pure . Map.adjust (\h -> h { _head_status = status }) name $ current

                  subsPerHead <- readMVar (_state_subscribers state)
                  case Map.lookup name subsPerHead of
                    Nothing -> pure ()
                    Just subs -> do
                      for_ subs $ \conn -> WS.sendTextData conn . Aeson.encode $ HeadStatusChanged name status
                  pure ()
                Nothing -> pure ()
              atomically $ writeTChan bRcvChan msg
        pure $ Node
          { _node_handle = processHndl
          , _node_info = nodeInfo
          , _node_monitor_thread = monitor
          , _node_send_msg = writeChan sndChan
          , _node_get_listen_chan = atomically $ dupTChan bRcvChan
          , _node_ports = ports
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
  -> (Int -> IO (Maybe [Int]))
  -> m (Map Address (ProcessHandle, HydraNodeInfo, [Int]))
startHydraNetwork sharedInfo actors getPorts = do
  liftIO $ createDirectoryIfMissing True "demo-logs"
  nodes :: (Map Address (HydraNodeInfo, [Int])) <- liftIO . fmap Map.fromList . mapM node $ zip [1 ..] (Map.toList actors)
  liftIO $ sequence . flip Map.mapWithKey nodes $ \name (node, ports) -> do
    logHndl <- openFile [iii|demo-logs/hydra-node-#{name}.log|] WriteMode
    errHndl <- openFile [iii|demo-logs/hydra-node-#{name}.error.log|] WriteMode
    let cp = (mkHydraNodeCP sharedInfo node (filter ((/= _nodeId node) . _nodeId) (fmap fst $ Map.elems nodes)))
             { std_out = UseHandle logHndl
             , std_err = UseHandle errHndl
             }
    (_,_,_,handle) <- createProcess cp
    pure (handle, node, ports)
  where
    node :: forall a. (Int, (a, HydraKeyInfo)) -> IO (a, (HydraNodeInfo, [Int]))
    node (n, (name, keys)) = do
      -- TODO: Handle lack of ports
      pd <- getTempPath'
      Just (ps@[p1,p2,p3]) <- getPorts 3
      pure ( name
           , (HydraNodeInfo n p1 p2 p3 keys pd, ps)
           )

data HydraSharedInfo = HydraSharedInfo
  { _hydraScriptsTxId :: String
  , _hydraLedgerProtocolParameters :: FilePath
  , _hydraLedgerGenesis :: FilePath
  , _hydraCardanoNodeInfo :: CardanoNodeInfo
  }
  deriving (Show, Read)

data HydraNodeInfo = HydraNodeInfo
  { _nodeId :: Int
  , _port :: Int
  -- ^ The port this node is running on
  , _apiPort :: Int
  -- ^ The port that this node is serving its pub/sub websockets api on
  , _monitoringPort :: Int
  , _keys :: HydraKeyInfo
  , _persistenceDir :: FilePath
  }
  deriving (Show,Read)

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
sharedArgs hsi =
  [ "--ledger-genesis"
  , _hydraLedgerGenesis hsi
  , "--ledger-protocol-parameters"
  , _hydraLedgerProtocolParameters hsi
  , "--hydra-scripts-tx-id"
  , _hydraScriptsTxId hsi
  ] <> cardanoNodeArgs (_hydraCardanoNodeInfo hsi)

nodeArgs :: HydraNodeInfo -> [String]
nodeArgs (HydraNodeInfo nodeId port apiPort monitoringPort
           (HydraKeyInfo
            (KeyPair cskPath _cvkPath)
            (KeyPair hskPath _hvkPath))
           persistenceDir) =
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
  , "--persistence-dir"
  , persistenceDir
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

signAndSubmitTx :: CardanoNodeInfo -> SigningKey -> Tx -> IO ()
signAndSubmitTx cninfo sk tx = do
  withTempFile "." "tx.draft" $ \draftFile draftHandle -> do
    LBS.hPut draftHandle $ Aeson.encode tx
    hClose draftHandle
    withTempFile "." "tx.signed" $ \signedFile signedHandle -> do
      hClose signedHandle
      let cp = (proc cardanoCliPath [ "transaction"
                                    , "sign"
                                    , "--tx-body-file"
                                    , draftFile
                                    , "--signing-key-file"
                                    , getSigningKeyFilePath sk
                                    , "--out-file"
                                    , signedFile
                                    ])
            { env = Just [( "CARDANO_NODE_SOCKET_PATH" , _nodeSocket cninfo)]
            }
      _ <- readCreateProcess cp ""
      submitTx cninfo signedFile >>= withLogging . waitForTxIn cninfo

websocketApiHandler :: MonadSnap m => State -> m ()
websocketApiHandler state = do
  runWebSocketsSnap $ \pendingConn -> do
    let
      apiKey = _state_apiKey state
      request = WS.pendingRequest pendingConn
      headers = WS.requestHeaders request

      authHeader :: CI BS.ByteString
      authHeader = "authorization"

      sentKey = lookup authHeader headers

      authenticated = Just apiKey == sentKey

    case authenticated of
      False -> WS.rejectRequest pendingConn "Invalid API Key"
      True -> do
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

handleTaggedMessage :: WS.Connection -> State -> Tagged ClientMsg -> IO (Tagged ServerMsg)
handleTaggedMessage conn state (Tagged tid msg) = do
  msg <- handleClientMessage conn state msg
  pure $ Tagged tid msg

versionStr :: Version
versionStr = "0.1.0"

handleClientMessage :: WS.Connection -> State -> ClientMsg -> IO (ServerMsg)
handleClientMessage conn state = \case
  GetStats -> do
    numHeads <- Map.size <$> readMVar (_state_heads state)
    networks <- readMVar (_state_networks state)

    let
      numNodes :: Int
      numNodes = Map.foldr (+) 0 . fmap (Map.size . _network_nodes) $ networks

    pure $ CurrentStats $ HydraPayStats numHeads numNodes

  RestartDevnet -> do
    cns <- readMVar (_state_cardanoNodeState state)
    case cardanoNodeState_nodeType cns of
      CfgPreview _ -> pure ()
      CfgDevnet -> do
        heads <- Map.keys <$> readMVar (_state_heads state)
        -- Terminate all the heads!
        for_ heads $ terminateHead state

        -- Create a new devnet and update the MVar
        modifyMVar_ (_state_cardanoNodeState state) $ \cns -> do
          teardownDevnet cns
          withLogging getCardanoNodeState
    pure DevnetRestarted

  GetDevnetAddresses amount -> do
    mAddrs <- getDevnetAddresses [1 .. amount]
    case mAddrs of
      Just addrs ->
        pure $ DevnetAddresses addrs
      Nothing ->
        pure $
        RequestError
        "Unable to open seeded address file, restart devnet or wait for seeding to complete"

  SubscribeTo name -> do
    modifyMVar_ (_state_subscribers state) $ pure . Map.insertWith (<>) name (pure conn)
    pure $ SubscriptionStarted name

  ClientHello -> pure $ ServerHello versionStr

  Withdraw address -> do
    -- Withdraw everything minus fees
    result <- withdraw state $ WithdrawRequest address Nothing
    case result of
      Right txid -> do
        cardanoNodeInfo <- stateCardanoNodeInfo state
        withLogging $ waitForTxIn cardanoNodeInfo $ txInput 0 txid
      _ -> pure ()
    pure $ either ServerError (const OperationSuccess) result

  GetAddTx txtype addr amount -> do
    result <- buildAddTx txtype state addr amount
    pure $ either ServerError FundsTx result

  CheckFuel addr -> do
    -- Calc the fuel amount
    fuel <- getProxyFuel state addr
    pure $ FuelAmount fuel

  CreateHead hc -> do
    result <- createHead state hc
    pure $ either ServerError (const OperationSuccess) result

  InitHead hi -> do
    result <- initHead state hi
    pure $ either ServerError (const OperationSuccess) result

  CommitHead hc -> do
    result <- commitToHead state hc
    pure $ either ServerError (const OperationSuccess) result

  CloseHead name -> do
    sendToHeadAndWaitFor Close (\case
      HeadIsFinalized {} -> True
      _ -> False) state name
    pure $ OperationSuccess

  DoesHeadExist name -> do
    result <- getHeadStatus state name
    pure $ HeadExistsResult $ case result of
      Left _ -> False
      Right _ -> True

  TearDownHead name -> do
    removeHead state name
    pure $ OperationSuccess

  _ -> pure $ UnhandledMessage

newtype HydraPayClient a = HydraPayClient
  { unHydraPayClient :: MaybeT (ReaderT ClientState IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

data ClientState = ClientState
  { clientState_connection :: WS.Connection
  , clientState_inbox :: TChan Msg
  , clientState_msgs :: TChan ServerMsg
  , clientState_nextId :: MVar Int
  }

data Msg
  = TaggedMsg (Tagged ServerMsg)
  | PlainMsg ServerMsg

instance FromJSON Msg where
  parseJSON v = (TaggedMsg <$> parseJSON v) <|> (PlainMsg <$> parseJSON v)

runHydraPayClient :: HydraPayClient a -> IO (Maybe a)
runHydraPayClient action = do
  nextId <- newMVar 0
  broadcastChannel <- newBroadcastTChanIO
  msgsChannel <- newBroadcastTChanIO
  WS.runClient "127.0.0.1" 8000 "hydra/api" $ \conn -> do
    -- We have a read thread that is centralized so we don't miss any messages
    _ <- forkIO $ forever $ do
      mMsg :: Maybe Msg <- Aeson.decode <$> WS.receiveData conn
      case mMsg of
        Just (PlainMsg p) -> do
          atomically $ writeTChan msgsChannel p
        Just msg ->
          atomically $ writeTChan broadcastChannel msg
        Nothing -> pure ()
      pure ()
    flip runReaderT (ClientState conn broadcastChannel msgsChannel  nextId) $ runMaybeT $ unHydraPayClient action

requestResponse :: ClientMsg -> HydraPayClient ServerMsg
requestResponse msg = HydraPayClient . MaybeT . ReaderT $ \(ClientState conn inbox otherInbox nid) -> do
  n <- modifyMVar nid $ \x -> pure (x + 1, x)
  readChan <- atomically $ dupTChan inbox
  WS.sendTextData conn . Aeson.encode $ Tagged n msg
  Just <$> waitForResponse n readChan
  where
    waitForResponse n chan = do
      msg <- atomically $ readTChan chan
      case msg of
        TaggedMsg (Tagged n' msg) | n == n' -> pure msg
        _ -> waitForResponse n chan

-- Start commiting
-- Some time after we start commiting, we will get a HeadIsOpen
-- at that point the Head is ready to go
-- We need to wait for this HeadIsOpen or some error or something
waitForHeadOpen :: HeadName -> HydraPayClient ()
waitForHeadOpen hname = HydraPayClient . MaybeT . ReaderT $ \(ClientState _ _ box nid) -> do
  putStrLn "Waiting for OPEN"
  readChan <- atomically $ dupTChan box
  checkForOpenStatus readChan
  liftIO $ putStrLn "DONE WE ARE OPEN"
  Just <$> pure ()
  where
    checkForOpenStatus box = do
      putStrLn "Waiting for a thing??"
      msg <- atomically $ readTChan box
      putStrLn "Did we even get this??"
      case msg of
        HeadStatusChanged name Status_Open | name == hname -> pure ()
        _ -> checkForOpenStatus box

getAddFundsTx :: Address -> Lovelace -> IO (Maybe Tx)
getAddFundsTx addr amount = do
  result <- runHydraPayClient $ requestResponse $ GetAddTx Funds addr amount
  pure $ case result of
    Just (FundsTx tx) -> Just tx
    _ -> Nothing
