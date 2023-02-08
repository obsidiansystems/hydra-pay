-- |
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HydraPay.Server where

import System.Exit (ExitCode(..))

import Crypto.Random
import qualified Data.ByteString.Base64 as Base64

import Control.Exception
import Snap.Core hiding (path, logError)
import Network.WebSockets.Snap
import qualified Network.WebSockets as WS

import Data.Int

import Data.Time
import Prelude hiding ((.))
import Control.Category ((.))
import System.Process
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Bool
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Aeson ((.:))
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Data.Maybe
import Data.String.Interpolate ( i, iii )
import System.IO ( IOMode(WriteMode), openFile, Handle, hClose )
import Network.WebSockets.Client

import Control.Monad.Reader
import Control.Monad.Fail

import Common.Helpers
import HydraPay.Logging

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


import Hydra.Types
import Hydra.Devnet
import Hydra.ClientInput
import qualified Hydra.Types as HT
import CardanoNodeInfo
import Hydra.ServerOutput as ServerOutput
import Control.Concurrent.STM (newBroadcastTChanIO, dupTChan, TChan)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Monad.Loops (untilJust, iterateUntilM)
import Data.Aeson.Types (parseMaybe)
import Control.Monad.Except
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (MaybeT))
import System.IO.Temp (withTempFile)
import Hydra.Snapshot as Snapshot
import qualified HydraPay.Config as Config
import HydraPay.Config (HydraPayMode(..), HydraPayConfig(_hydraPayMode))

type ApiKey = T.Text

-- | The location where we store cardano and hydra keys
getKeyPath :: IO FilePath
getKeyPath = do
  createDirectoryIfMissing True path
  pure path
  where
    path = "keys"

data Instance = Instance
  { instance_apiKey :: ApiKey
  , instance_connections :: [WS.Connection]
  , instance_heads :: Map HeadName Head
  , instance_networks :: Map HeadName Network
  , instance_subscribers :: Map HeadName [WS.Connection]
  }

-- | State we need to run/manage Heads
data State = State
  { _state_cardanoNodeState :: MVar CardanoNodeState
  -- ^ The process handles of the try-out/livedoc Cardano devnet node, if any.
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
  , _state_apiKeys :: [BS.ByteString]
  , _state_config :: HydraPayConfig

  , _state_connectedClients :: MVar [(WS.Connection, ApiKey)]
  , _state_instances :: MVar (Map ApiKey Instance)
  , _state_instanceAddresses :: MVar (Map ApiKey [(Address, KeyPair)])
  , _state_nextClearTime :: MVar UTCTime
  }

-- The Process of the node and the HydraSharedInfo for that node
data CardanoNodeState = CardanoNodeState
  { cardanoNodeState_processHandles :: Maybe ProcessHandles
  , cardanoNodeState_hydraSharedInfo :: HydraSharedInfo
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

runningDevnet :: MonadIO m => State -> m Bool
runningDevnet state =
  pure $ case getHydraPayMode state of
    ManagedDevnetMode -> True
    _ -> False

apiKeyConfigLocation :: T.Text
apiKeyConfigLocation = "backend/api-keys"

runHydraPay :: HydraPayConfig -> (State -> IO a) -> IO a
runHydraPay cfg action = do
  configs <- ConfigIO.getConfigs
  result <- runConfigsT configs $ runMaybeT $
    MaybeT $ getConfig apiKeyConfigLocation

  keys <- case (fmap (fmap LBS.toStrict . LBS.lines . LBS.fromStrict) result) of
    Nothing -> do
      -- Generate a key if we don't have one
      withLogging $ logInfo "No API Keys found, generating..."
      createDirectoryIfMissing True "config/backend"
      newKeys <- for [0 .. 100] $ \_ -> do
        Base64.encode <$> getRandomBytes 32
      let
          configFullPath = ("config/" <> T.unpack apiKeyConfigLocation)
      BS.writeFile configFullPath $ LBS.toStrict $ LBS.unlines $ fmap LBS.fromStrict newKeys
      withLogging $ logInfo . pretty $ "API Keys are available in " <> configFullPath
      pure newKeys
    Just keys ->
      pure keys

  addrs <- liftIO $ newMVar mempty
  subs <- liftIO $ newMVar mempty

  heads <- liftIO $ newMVar mempty
  networks <- liftIO $ newMVar mempty
  path <- liftIO getKeyPath
  ports <- liftIO $ newMVar ([9000..11000] :: [Int])
  let getPorts n = modifyMVar ports $ \ps -> pure $
        let (xs,ys) = splitAt n ps
        in if null ys
           then (ps, Nothing)
           else (ys, Just xs)
  let freePorts ps = void $ modifyMVar_ ports (pure . (ps ++))

  cardanNodeResult <- withLogging (makeCardanoNodeStateAndRestartDemoDevnet (_hydraPayMode cfg))

  let
    mkEmptyInstance key = Instance key [] mempty mempty mempty
    initialInstances = Map.fromList $ fmap ((\k -> (k, mkEmptyInstance k)) . T.decodeUtf8) keys

  instances <- newMVar initialInstances
  instanceAddresses <- newMVar mempty
  time <- getCurrentTime

  let
    devnetLifetime = 120

  timeVar <- newMVar $ addUTCTime (fromIntegral devnetLifetime) time
  clients <- newMVar mempty

  case cardanNodeResult of
    Left err -> do
      withLogging $ logError $ pretty err
      error err
    Right nodeState -> do
      cardanoNodeState <- newMVar nodeState
      let
        state = State cardanoNodeState addrs heads networks subs path getPorts freePorts keys cfg clients instances instanceAddresses timeVar
        fullAction = do
          forkIO $ forever $ do
            threadDelay $ seconds devnetLifetime
            instances <- liftIO $ readMVar (_state_instances state)
            for instances $ \is -> do
              -- NOTE(skylar): Remember terminating the head removes the network!
              for_ (Map.keys $ instance_heads is) $ terminateHead state $ instance_apiKey is

            -- Create a new devnet and update the MVar
            liftIO $ modifyMVar (_state_cardanoNodeState state) $ \oldCns -> do
              teardownDevnet oldCns
              result <- withLogging (makeCardanoNodeStateAndRestartDemoDevnet (getHydraPayMode state))
              case result of
                Right newstate -> pure (newstate, DevnetRestarted)
                Left err -> pure (oldCns, ApiError $ "Failed to restart devnet: " <> T.pack err)

            modifyMVar_ (_state_instances state) $ pure . (const initialInstances)

            -- This purges old connections
            broadcastToAll state DevnetRestarted

            -- Reseed addresses
            clients <- readMVar (_state_connectedClients state)
            for_ (fmap snd clients) $ \client -> do
              noAddresses <- withMVar (_state_instanceAddresses state) $ pure . maybe True null . Map.lookup client
              when noAddresses $ do--void $ forkIO $ do
                nodeInfo <- withMVar (_state_cardanoNodeState state) (pure .  _hydraCardanoNodeInfo . cardanoNodeState_hydraSharedInfo)
                addresses <- withLogging $ seedTestAddresses nodeInfo devnetFaucetKeys 4
                modifyMVar_ (_state_instanceAddresses state) $ pure . Map.insert client addresses

            nextTime <- getCurrentTime
            modifyMVar_ timeVar $ pure . const (addUTCTime (fromIntegral devnetLifetime) nextTime)

          action state
      finally (action state) $ do
        node <- readMVar cardanoNodeState
        stopCardanoNode node

-- | If we're in 'LiveDocMode' delete the devnet and restart it.
makeCardanoNodeStateAndRestartDemoDevnet :: (MonadIO m) => HydraPayMode -> m (Either String CardanoNodeState)
makeCardanoNodeStateAndRestartDemoDevnet = \case
  ManagedDevnetMode -> liftIO $ runExceptT $ do
      lift $ do
        removePathForcibly "devnet"
        removePathForcibly nodeLogDir
        withLogging prepareDevnet
      handles <- lift $ createProcess cardanoNodeCreateProcess
      lift $ threadDelay (seconds 3)
      hydraSharedInfo <- do
        scriptsTxId <- ExceptT $ withLogging $ publishReferenceScripts cardanoDevnetNodeInfo (_signingKey devnetFaucetKeys)
        pure $ HydraSharedInfo
          { _hydraScriptsTxId = T.unpack scriptsTxId,
            _hydraLedgerGenesis = "devnet/genesis-shelley.json",
            _hydraLedgerProtocolParameters = "devnet/hydra-protocol-parameters.json",
            _hydraCardanoNodeInfo = cardanoDevnetNodeInfo
          }
      liftIO $ writeProtocolParameters (_hydraCardanoNodeInfo hydraSharedInfo)
--      _ <- lift $ withLogging $ seedTestAddresses (_hydraCardanoNodeInfo hydraSharedInfo) devnetFaucetKeys (4 * 100)
      pure $ CardanoNodeState (Just handles) hydraSharedInfo

  ConfiguredMode cardanoParams hydraParams -> do
    protocolParametersPath <- liftIO $ snd <$> getTempPath
    let cninf = CardanoNodeInfo { _nodeType = TestNet (Config._testnetMagic cardanoParams),
                             _nodeSocket = Config._nodeSocket cardanoParams,
                             _nodeLedgerProtocolParameters = protocolParametersPath,
                             _nodeLedgerGenesis = Config._ledgerGenesis cardanoParams
                           }
    liftIO $ writeProtocolParameters cninf
    pure $ Right $ CardanoNodeState Nothing $ HydraSharedInfo
         { _hydraScriptsTxId = Config._hydraScriptsTxId hydraParams,
           _hydraLedgerGenesis = Config._hydraLedgerGenesis hydraParams,
           _hydraLedgerProtocolParameters = Config._hydraLedgerProtocolParameters hydraParams,
           _hydraCardanoNodeInfo = cninf
         }

getHydraSharedInfo :: MonadIO m => State -> m HydraSharedInfo
getHydraSharedInfo = liftIO . fmap cardanoNodeState_hydraSharedInfo . readMVar . _state_cardanoNodeState

getCardanoNodeInfo :: MonadIO m => State -> m CardanoNodeInfo
getCardanoNodeInfo = fmap _hydraCardanoNodeInfo . getHydraSharedInfo

getHydraPayMode :: State -> HydraPayMode
getHydraPayMode = Config._hydraPayMode . _state_config

websocketApiHandler :: MonadSnap m => State -> m ()
websocketApiHandler state = do
  runWebSocketsSnap $ \pendingConn -> do
  authVar <- newMVar Nothing
  conn <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 (pure ()) $ do
    WS.sendTextData conn . Aeson.encode $ ServerHello versionStr
    forever $ do
      -- To provide good Error UX if somebody has a tagged message that fails to parse, we want
      -- to show them why, so we decode in two steps so we can tag our response with the error message
      -- ensuring it gets routed to the correct place!
      decodeResult :: Either String (Tagged Value) <- Aeson.eitherDecode <$> WS.receiveData conn
      who <- readMVar authVar
      WS.sendTextData conn =<< case decodeResult of
        Left err -> pure . Aeson.encode $ InvalidMessage $ T.pack err
        Right (Tagged t val) ->
          case (who, Aeson.parseEither parseJSON val) of
            -- Handle authentication
            (_, Right (Authenticate token)) -> do
              let apiKeys = _state_apiKeys state
                  sentKey = T.encodeUtf8 token
                  authResult = any (sentKey ==) apiKeys
              modifyMVar_ authVar (\_ -> pure $ bool Nothing (Just token) authResult)
              modifyMVar_ (_state_connectedClients state) $ pure . ((conn, token) :)
              -- NOTE(skylar): Thread to populate funded addresses for each instance
              noAddresses <- withMVar (_state_instanceAddresses state) $ pure . maybe True null . Map.lookup token
              when noAddresses $ do--void $ forkIO $ do
                nodeInfo <- withMVar (_state_cardanoNodeState state) (pure .  _hydraCardanoNodeInfo . cardanoNodeState_hydraSharedInfo)
                addresses <- withLogging $ seedTestAddresses nodeInfo devnetFaucetKeys 4
                -- for_ addresses $ \(UnsafeToAddress a) -> putStrLn $ T.unpack a
                modifyMVar_ (_state_instanceAddresses state) $ pure . Map.insert token addresses
              pure . Aeson.encode . Tagged t $ AuthResult authResult
            -- TODO(skylar): Re-introduce this
            -- Allow GetIsManagedDevnet even when not authenticated to allow the
            -- live docs to make this distinction upon loading.
--            (_, Right GetIsManagedDevnet) ->
--              fmap Aeson.encode $ withLogging $ handleTaggedMessage conn apiKey state (Tagged t GetIsManagedDevnet)
            -- Turn away requests that aren't authenticated
            (Nothing, Right _) ->
              pure . Aeson.encode . Tagged t $ NotAuthenticated
            -- When authenticated just process as normal
            (Just apiKey, Right clientMsg) ->
              fmap Aeson.encode $ withLogging $ handleTaggedMessage conn apiKey state $ Tagged t clientMsg
            (_, Left err) ->
              pure . Aeson.encode $ Tagged t $ InvalidMessage $ T.pack err

stopCardanoNode :: CardanoNodeState -> IO ()
stopCardanoNode (CardanoNodeState handles _) =
  maybe (pure ()) cleanupProcess handles

teardownDevnet :: CardanoNodeState -> IO ()
teardownDevnet cnode = do
  stopCardanoNode cnode
  exists <- doesPathExist "devnet"
  when exists $ removePathForcibly "devnet"

headBalance :: (MonadIO m) => State -> ApiKey -> HeadName -> Address -> m (Either HydraPayError Lovelace)
headBalance state key name addr =
  withNode state key name addr $ \node proxyAddr -> do
  utxos <- getNodeUtxos node proxyAddr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
    fullAmount = toInteger $ sum txInAmounts
  pure (Right fullAmount)

headAllBalances :: (MonadIO m) => State -> ApiKey -> HeadName -> m (Either HydraPayError (Map Address Lovelace))
headAllBalances state key headname = do
  mHead <- lookupHead state key headname
  case mHead of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just (Head _ participants _ _) ->
      withAnyNode state key headname $ \node -> do
      liftIO $ _node_send_msg node GetUTxO
      c <- liftIO $ _node_get_listen_chan node
      untilJust $ do
        x <- liftIO . atomically $ readTChan c
        case x of
          GetUTxOResponse utxoz ->
            Just . mapLeft ProcessError <$> reverseMapProxyBalances state participants (wholeUtxoToBalanceMap utxoz)

          CommandFailed {} -> pure $ Just $ Right mempty
          _ -> pure Nothing

-- | Take all the UTxOs and organize them into a map of lovelace balances: Map Address Lovelace
wholeUtxoToBalanceMap :: WholeUTXO -> Map Address Lovelace
wholeUtxoToBalanceMap =
   foldr (Map.unionWith (+)) mempty . fmap singletonFromTxInfo . Map.elems

reverseMapProxyBalances :: MonadIO m => State -> Set Address -> Map Address Lovelace -> m (Either String (Map Address Lovelace))
reverseMapProxyBalances state participants inHeadBalance = runExceptT $ do
  reverseMap :: Map Address Address <- fmap mconcat $ for (Set.toList participants) $ \paddr -> do
    (proxyAddr, _) <- ExceptT $ addOrGetKeyInfo state paddr
    pure $ Map.singleton proxyAddr paddr

  pure $ Map.mapKeys (\proxyKey-> fromMaybe proxyKey $ Map.lookup proxyKey reverseMap) inHeadBalance


singletonFromTxInfo :: TxInInfo -> Map Address Lovelace
singletonFromTxInfo (TxInInfo addr _ val) =
  Map.singleton addr $ fromIntegral lovelace
  where
    lovelace = fromMaybe 0 . Map.lookup "lovelace" $ val


l1Balance :: (MonadIO m) => State -> Address -> Bool -> m Lovelace
l1Balance state addr includeFuel = do
  cninf <- getCardanoNodeInfo state
  utxos <- queryAddressUTXOs cninf addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ bool filterOutFuel id includeFuel utxos
    fullAmount = toInteger $ sum txInAmounts
  pure fullAmount

fuelBalance :: (MonadIO m) => State -> Address -> m Lovelace
fuelBalance state addr = do
  cninf <- getCardanoNodeInfo state
  utxos <- queryAddressUTXOs cninf addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ filterFuel utxos
    fullAmount = toInteger $ sum txInAmounts
  pure fullAmount

-- | The balance of the proxy address associated with the address given
getProxyFunds :: (MonadIO m) => State -> Address -> m (Either String Lovelace)
getProxyFunds state addr = runExceptT $ do
  (proxyAddr, _) <- ExceptT $ addOrGetKeyInfo state addr
  l1Balance state proxyAddr False

getProxyFuel :: (MonadIO m) => State -> Address -> m (Either String Lovelace)
getProxyFuel state addr = runExceptT $ do
  (proxyAddr, _) <- ExceptT $ addOrGetKeyInfo state addr
  cninf <- lift $ getCardanoNodeInfo state
  utxos <- lift $ queryAddressUTXOs cninf proxyAddr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) $ filterFuel utxos
    fullAmount = toInteger $ sum txInAmounts
  pure fullAmount

buildAddTx :: MonadIO m => TxType -> State -> Address -> Lovelace -> m (Either String Tx)
buildAddTx tType state fromAddr lovelaceAmount = runExceptT $ do
  cardanoNodeInfo <- lift $ getCardanoNodeInfo state
  utxos <- lift $ queryAddressUTXOs cardanoNodeInfo fromAddr
  let
    txInLovelaceAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
    lovelaceTotal = fromIntegral $ sum txInLovelaceAmounts

  -- Better errors before we hit cardano-cli
  when (Map.null txInLovelaceAmounts) $ throwError "This address has no ada"
  when (lovelaceTotal < lovelaceAmount) $ throwError $
    "Insufficient funds, wanted " <> show lovelaceAmount <> ", current balance " <> show lovelaceTotal
  when (lovelaceTotal == lovelaceAmount) $ throwError $
    "No room for fees, transaction won't balance: wanted " <> show lovelaceAmount <> ", current balance " <> show lovelaceTotal

  (toAddr, _) <- ExceptT $ addOrGetKeyInfo state fromAddr

  txBodyPath <- lift $ liftIO $ snd <$> getTempPath
  _ <- ExceptT $ processAdapter $ liftIO $ readCreateProcessWithExitCode (proc cardanoCliPath
                       (filter (/= "") $ [ "transaction"
                        , "build"
                        , "--babbage-era"
                        , "--cardano-mode"
                        ]
                        <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) . Map.keys $ txInLovelaceAmounts)
                        <>
                        [ "--tx-out"
                        , [i|#{addressToString toAddr}+#{lovelaceAmount}|]
                        ]
                        <> bool [] [ "--tx-out-datum-hash", T.unpack fuelMarkerDatumHash ] (isFuelType tType)
                        <>
                        [ "--change-address"
                        , addressToString fromAddr
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
    Just tx -> pure tx
    Nothing -> throwError $ "Failed to read tx file at " <> txBodyPath
      -- pure $ Left $ "Failed to read tx file at " <> T.pack txBodyPath

withNetwork ::
  MonadIO m =>
  State ->
  ApiKey ->
  HeadName ->
  (Network -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withNetwork state key name f = maybe (pure $ Left NetworkIsn'tRunning) f =<< getNetwork state key name

withNode ::
  MonadIO m =>
  State ->
  ApiKey ->
  HeadName ->
  Address ->
  (Node -> Address -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withNode state key name addr f =
  withNetwork state key name $ \network ->
  runExceptT $ do
  (proxyAddr, _) <- ExceptT $ mapLeft ProcessError <$> addOrGetKeyInfo state addr
  case Map.lookup proxyAddr $ _network_nodes network of
    Nothing -> throwError NotAParticipant
    Just node -> ExceptT $ f node proxyAddr

withAnyNode ::
  MonadIO m =>
  State ->
  ApiKey ->
  HeadName ->
  (Node -> m (Either HydraPayError b)) ->
  m (Either HydraPayError b)
withAnyNode state key name f =
  withNetwork state key name $ \network -> do
  case headMay $ Map.elems (_network_nodes network) of
    Nothing -> pure $ Left NotAParticipant
    Just node -> f node

initHead :: MonadIO m => State -> ApiKey -> HeadInit -> m (Either T.Text ())
initHead state key (HeadInit name con) = do
  result <- sendToHeadAndWaitForOutput (Init $ fromIntegral con) (\case
    ReadyToCommit {} -> True
    PostTxOnChainFailed {} -> True
    _ -> False) state key name

  case result of
    Right ReadyToCommit {} -> pure $ Right ()
    Right PostTxOnChainFailed {} -> pure $ Left "Failed to submit tx, check participants fuel"
    Left err -> pure $ Left $ tShow err
    _ -> pure $ Left "Impossible error"

data WithdrawRequest = WithdrawRequest
  { withdraw_address :: Address
  , withdraw_amount :: Maybe Lovelace
  -- ^ Nothing to withdraw all.
  , withdraw_takeFuel :: Bool
  }
  deriving(Eq, Show, Generic)

instance ToJSON WithdrawRequest
instance FromJSON WithdrawRequest


-- | Withdraw funds (if available from proxy request), fee is subtracted.
withdraw :: MonadIO m => State -> WithdrawRequest -> m (Either HydraPayError TxId)
withdraw state (WithdrawRequest addr maybeAmount takeFuel) = runExceptT $ do
  cninf <- lift $ getCardanoNodeInfo state
  (proxyAddr, keyInfo) <- ExceptT $ mapLeft ProcessError <$> addOrGetKeyInfo state addr
  utxos <- lift $ queryAddressUTXOs cninf proxyAddr
  let
    filterFunc = bool filterOutFuel id takeFuel
    filtered = filterFunc utxos
    txInAmounts = fmap toInteger $ Map.mapMaybe (Map.lookup "lovelace" . HT.value) filtered
  let signingKey = _signingKey $ _cardanoKeys keyInfo
  ExceptT $ fmap (mapLeft ProcessError) $
    liftIO $ transferAmount cninf signingKey txInAmounts addr True maybeAmount

fanoutHead :: MonadIO m => State -> ApiKey -> HeadName -> m (Either HydraPayError HeadStatus)
fanoutHead = sendToHeadAndWaitFor Fanout $ \case
  HeadIsFinalized {} -> True
  _ -> False

closeHead :: MonadIO m => State -> ApiKey -> HeadName -> m (Either HydraPayError ())
closeHead state key name = runExceptT $ do
  HeadStatus _ _ status _ <- ExceptT $ getHeadStatus state key name
  let
    getInput = \case
      Status_Init -> pure Abort
      Status_Committing -> pure Abort

      Status_Open -> pure Close

      Status_Pending -> throwError $ ProcessError "Head is stil pending, you can just terminate/remove it."
      Status_Closed -> throwError $ ProcessError "Head is already closed."
      Status_Fanout -> throwError $ ProcessError "Head is fanning out, please wait for it to finish and then simply remove the head."
      Status_Finalized -> throwError $ ProcessError "Head is finalized, you can just remove it."
      Status_Aborted -> throwError $ ProcessError "Head is aborted, you can just remove it."

  action <- getInput status
  output <- ExceptT $ sendToHeadAndWaitForOutput action waitingFor state key name
  case output of
    CommandFailed {} -> throwError $ ProcessError "Command failed, please check Head Status"
    HeadIsClosed {} -> pure ()
    HeadIsAborted {} -> pure ()
    _ -> throwError $ ProcessError "Internal error: Unexpected command receieved"
  where
    waitingFor = \case
      HeadIsAborted {} -> True
      HeadIsClosed {} -> True
      CommandFailed {} -> True
      _ -> False

sendToHeadAndWaitFor :: MonadIO m => ClientInput -> (ServerOutput Value -> Bool) -> State -> ApiKey -> HeadName -> m (Either HydraPayError HeadStatus)
sendToHeadAndWaitFor ci fso state key headName = do
  mNetwork <- getNetwork state key headName
  case mNetwork of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just network -> do
      let firstNode = head $ Map.elems $ _network_nodes network
          sendMsg = _node_send_msg firstNode
          getChan = _node_get_listen_chan firstNode
      liftIO $ do
        channel <- getChan
        sendMsg ci
        let
          waitForCloseHandler = do
            output <- atomically $ readTChan channel
            if fso output then pure () else waitForCloseHandler
        waitForCloseHandler
      getHeadStatus state key headName

sendToHeadAndWaitForOutput :: MonadIO m => ClientInput -> (ServerOutput Value -> Bool) -> State -> ApiKey -> HeadName -> m (Either HydraPayError (ServerOutput Value))
sendToHeadAndWaitForOutput ci fso state key headName = do
  mNetwork <- getNetwork state key headName
  case mNetwork of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just network -> do
      let firstNode = head $ Map.elems $ _network_nodes network
          sendMsg = _node_send_msg firstNode
          getChan = _node_get_listen_chan firstNode
      liftIO $ do
        channel <- getChan
        sendMsg ci
        let
          waitForCloseHandler = do
            output <- atomically $ readTChan channel
            if fso output then pure $ Right output else waitForCloseHandler
        waitForCloseHandler

commitToHead :: MonadIO m => State -> ApiKey -> HeadCommit -> m (Either HydraPayError ())
commitToHead state key (HeadCommit name addr lovelaceAmount) =
  withNode state key name addr $ \node proxyAddr -> do

  let
    rightLovelaceAmount (TxInInfo _ _ val) =
      Map.lookup "lovelace" val == Just (fromIntegral lovelaceAmount)

  cardanoNodeInfo <- getCardanoNodeInfo state
  -- Get the first UTXO that has the correct lovelaceAmount as we MUST commit 1 or no UTXOs
  proxyFunds <- Map.take 1 . Map.filter rightLovelaceAmount . filterOutFuel <$> queryAddressUTXOs cardanoNodeInfo proxyAddr

  case Map.null proxyFunds of
    True -> pure $ Left NoValidUTXOToCommit
    False ->
      sendToNodeAndListen node (Commit proxyFunds) (\case
      Committed {} -> Just $ Right ()
      PostTxOnChainFailed {} -> Just $ Left NodeCommandFailed
      CommandFailed {} -> Just $ Left NodeCommandFailed
      _ -> Nothing)
      where
        sendToNodeAndListen n ci fso = do
          let
            sendMsg = _node_send_msg n
            getChan = _node_get_listen_chan n

          liftIO $ do
            channel <- getChan
            sendMsg ci
            let
              waitForCloseHandler = do
                output <- atomically $ readTChan channel
                case fso output of
                  Just x  -> pure x
                  _ -> waitForCloseHandler
            waitForCloseHandler

-- | Create a head by starting a Hydra network.
createHead :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => State -> ApiKey -> HeadCreate -> m (Either HydraPayError Head)
createHead state key (HeadCreate name participants) = runExceptT $ do
  when (null participants) $ throwError NotEnoughParticipants
  headExists <- isJust <$> lookupHead state key name
  when headExists $ throwError $ HeadExists name
  statusBTChan <- liftIO newBroadcastTChanIO
  let potentialHydraHead = Head name (Set.fromList participants) Status_Pending statusBTChan
  startResult <- startNetwork state key potentialHydraHead
  case startResult of
    Right network -> do
      logInfo $ pretty $ "Network started for head \"" <> name <> "\""
      liftIO $ modifyMVar_ (_state_instances state) $ \iss -> do
        pure $ Map.adjust (\is -> is { instance_heads = Map.insert name potentialHydraHead $ instance_heads is
                                     -- TODO(skylar): This should already be added
                                     --, instance_networks = Map.insert name network $ instance_networks is
                                     } ) key iss
      pure potentialHydraHead
    Left err -> do
      lift $ logError $ pretty $ "Failed to start network for head \"" <> name <> "\": " <> T.pack err
      throwError $ ProcessError err

data HydraTxError =
  HydraTxNotSeen

filterUtxos :: Address -> WholeUTXO -> WholeUTXO
filterUtxos addr = Map.filter ((== addr) . HT.address)


getNodeUtxos :: MonadIO m => Node -> Address -> m (Map TxIn TxInInfo)
getNodeUtxos node proxyAddr = do
  liftIO $ _node_send_msg node GetUTxO
  c <- liftIO $ _node_get_listen_chan node
  untilJust $ do
    x <- liftIO . atomically $ readTChan c
    case x of
      GetUTxOResponse utxoz ->
        pure $ Just (filterUtxos proxyAddr utxoz)
      CommandFailed {} -> pure $ Just mempty
      _ -> pure Nothing


submitTxOnHead :: (MonadIO m) => State -> ApiKey -> Address -> HeadSubmitTx -> m (Either HydraPayError Pico)
submitTxOnHead state key addr (HeadSubmitTx name toAddr lovelaceAmount) =
  withNode state key name addr $ \node proxyAddr -> runExceptT $ do
  c <- lift $ liftIO $ _node_get_listen_chan node
  senderUtxos :: Map TxIn TxInInfo <- lift $ getNodeUtxos node proxyAddr
  let signingKey = _signingKey . _cardanoKeys . _keys . _node_info $ node
  let lovelaceUtxos = toInteger <$> Map.mapMaybe (Map.lookup "lovelace" . HT.value) senderUtxos
  (toAddrProxy, _) <- ExceptT $ mapLeft ProcessError <$> addOrGetKeyInfo state toAddr
  (txid, signedTxJsonStr) <- ExceptT $ fmap (mapLeft ProcessError) $ liftIO $ buildSignedHydraTx signingKey proxyAddr toAddrProxy lovelaceUtxos lovelaceAmount
  let jsonTx :: Aeson.Value = fromMaybe (error "Failed to parse TX") . Aeson.decode . LBS.pack $ signedTxJsonStr
  let txCborHexStr = fromJust . parseMaybe (withObject "signed tx" (.: "cborHex")) $ jsonTx

  startTime <- lift $ liftIO getCurrentTime
  liftIO $ _node_send_msg node $ NewTx . T.pack $ txCborHexStr
  untilJust $ do
    x <- liftIO . atomically $ readTChan c
    case x of
      SnapshotConfirmed { snapshot = Snapshot { Snapshot.confirmedTransactions = confirmeds } } -> do
        let confirmedIds = mapMaybe (parseMaybe (withObject "tx" (.: "id"))) confirmeds
        if txid `elem` confirmedIds
          then do
            endTime <- liftIO getCurrentTime
            pure . Just $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
          else pure Nothing

      ServerOutput.TxInvalid theUtxo tx valError -> do
        let
          parsedError = HydraPay.Api.TxInvalid theUtxo tx valError <$ (guard . (== txid) =<< parseMaybe (withObject "tx" (.: "id")) tx)
        _ <- throwError $ fromMaybe (ProcessError "Tx Invalid") parsedError
        pure Nothing
      _ -> pure Nothing

-- | Send a Server Message to all subscribers of a particular Head
broadcastToSubscribers :: MonadIO m => State -> ApiKey -> HeadName -> ServerMsg -> m ()
broadcastToSubscribers state key hname msg = do
  subsPerHead <- liftIO $ withMVar (_state_instances state) $ pure . maybe mempty instance_subscribers . Map.lookup key
  case Map.lookup hname subsPerHead of
    Nothing -> pure ()
    Just subs -> liftIO $
      for_ subs $ \subConn ->
      WS.sendTextData subConn . Aeson.encode $ msg

-- | Broadcast to all connected clients
broadcastToAll :: MonadIO m => State -> ServerMsg -> m ()
broadcastToAll state msg = liftIO $ do
  modifyMVar_ (_state_connectedClients state) $ \connected -> do
    fmap catMaybes $ for connected $ \(conn, key) -> do
      err  :: (Either SomeException ()) <- try $ WS.sendTextData conn . Aeson.encode $ msg
      case err of
        -- Something went wrong with this connection we should remove it
        Left _ -> do
          liftIO $ putStrLn "Failed to send to client"
          pure $ Nothing
        _ -> pure $ Just (conn, key)

removeHead :: (MonadIO m) => State -> ApiKey -> HeadName -> m (Either String ())
removeHead state key name = runExceptT $ do
  status <- ExceptT $ fmap (mapLeft show) $ getHeadStatus state key name
  case headStatus_status status of
    Status_Init -> throwError "Head is Initialized, please Close the head before removing to avoid losing funds."
    Status_Committing -> throwError "Head is Committing, please Close the head before removing to avoid losing funds."
    Status_Open -> throwError "Head is Open, please Close the head before removing to avoid losing funds."
    Status_Closed -> throwError "Head is closing, please wait for the contestation period to be over and the fanout to complete."
    Status_Fanout -> throwError "Head is waiting for fanout transaction to complete."
    -- Head is pending or finalized this is fine.
    _ -> do
      lift $ terminateHead state key name
      liftIO $ modifyMVar_ (_state_instances state) $ \iss -> do
        pure $ Map.adjust (\inst -> inst { instance_heads = Map.delete name $ instance_heads inst }) key iss
      pure ()

terminateHead :: (MonadIO m) => State -> ApiKey -> HeadName -> m ()
terminateHead state key headName =
  liftIO $ modifyMVar_ (_state_instances state) $ \is -> do
    let
      mNetwork :: Maybe Network = (Map.lookup headName . instance_networks <=< Map.lookup key) $ is
    case mNetwork of
      Just network -> do
        terminateNetwork state key network
        pure $ Map.adjust (\inst -> inst { instance_networks = Map.delete headName (instance_networks inst) }) key is
      Nothing ->
        pure is

terminateNetwork :: MonadIO m => State -> ApiKey -> Network -> m ()
terminateNetwork state key n = liftIO $ do
  mapM_ (terminateProcess . _node_handle) . _network_nodes $ n
  _state_freePorts state . concatMap _node_ports . Map.elems . _network_nodes $ n

-- | Lookup head via name
lookupHead :: MonadIO m => State -> ApiKey -> HeadName -> m (Maybe Head)
lookupHead state key name = do
  liftIO $ withMVar (_state_instances state) (pure . (Map.lookup name . instance_heads <=< Map.lookup key))

-- NOTE(skylar): Idempotent
-- | Generate a proxy address with keys,
addOrGetKeyInfo :: MonadIO m => State -> Address -> m (Either String (Address, HydraKeyInfo))
addOrGetKeyInfo state addr = do
  cardanoNodeInfo <- getCardanoNodeInfo state
  liftIO $ modifyMVar (_state_proxyAddresses state) $ \old -> do
    case Map.lookup addr old of
      Just info -> pure (old, Right info)
      Nothing -> do
        liftIO $ createDirectoryIfMissing False keystore
        result <- runExceptT $ do
          keyInfo <- ExceptT $ withLogging $ generateKeysIn $ keystore <> "/" <> addressToString addr <> "-proxy"
          proxyAddress <- ExceptT $ liftIO
                          $ getCardanoAddress cardanoNodeInfo
                          $ _verificationKey . _cardanoKeys $ keyInfo
          pure (proxyAddress, keyInfo)
        case result of
          Left err -> pure (old, Left err)
          Right info ->
            pure (Map.insert addr info old, Right info)
    where
      keystore = "keystore"

getHeadStatus :: MonadIO m => State -> ApiKey -> HeadName -> m (Either HydraPayError HeadStatus)
getHeadStatus state key name = liftIO $ do
  mHead <- lookupHead state key name
  case mHead of
    Nothing -> pure $ Left HeadDoesn'tExist
    Just (Head hname participants status _) -> do
      running <- isJust <$> getNetwork state key hname

      -- Balances only really exist/are relevant during the Open phase
      -- don't even query them otherwise
      balances <- case status of
        Status_Open ->
          headAllBalances state key name
        _ ->
          pure $ Right $ Map.fromList $ (,0) <$> Set.toList participants

      pure $ HeadStatus hname running status <$> balances


-- | Wait for a head to change to or past a state. Returns 'Nothing'
-- if the head was not found, otherwise 'Just SomeStatus'.
waitForHeadStatus :: (MonadIO m) => State -> ApiKey -> HeadName -> Status -> m (Maybe Status)
waitForHeadStatus state key name status = runMaybeT $ do
  hd <- MaybeT $ lookupHead state key name
  c <- liftIO . atomically $ dupTChan $ _head_status_bchan hd
  if _head_status hd >= status
    then pure $ _head_status hd
    else untilJust $ do
      x <- liftIO . atomically $ readTChan c
      if x >= status
        then pure $ Just x
        else pure Nothing

-- | Start a network for a given Head, trying to start a network that already exists is a no-op and you will just get the existing network
startNetwork :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => State -> ApiKey -> Head -> m (Either String Network)
startNetwork state key (Head name participants _ statusBTChan) = do
  mNetwork <- getNetwork state key name
  case mNetwork of
    Just network -> pure $ Right network
    Nothing -> runExceptT $ do
      -- Hydra Node has a hard limit of 4 participants, we shouldn't go through the seance of spawning a network when this is known information
      when (Set.size participants > 4) $ throwError "Hydra Node Error: Maximum number of parties is currently set to: 4"
      proxyMap <- ExceptT $ participantsToProxyMap state participants
      hydraInfo <-lift $ getHydraSharedInfo state
      nodes <- lift $ startHydraNetwork hydraInfo proxyMap (_state_getPorts state)
      let numNodes = length nodes
      readyCounterChan <- liftIO newChan
      network <- fmap Network . forM nodes $ \(processHndl, nodeInfo, ports) -> do
        let thePort =  _apiPort nodeInfo
        bRcvChan <- liftIO newBroadcastTChanIO
        sndChan <- liftIO newChan
        monitor <- liftIO $ forkIO $ withLogging $ do
          liftIO $ threadDelay 3000000
          logInfo [i|Connecting to WS port #{thePort} and other stuff|]
          liftIO $ runClient "127.0.0.1" thePort "/" $ \conn -> withLogging $ do
            logInfo [i|Connected to node on port #{thePort}|]
            -- Fork off "message send" thread
            liftIO $ void $ forkIO $ withLogging $ forever $ do
              toSnd :: ClientInput <- liftIO $ readChan sndChan
              logInfo [i|Sending to WS port #{thePort}: #{show toSnd}|]
              liftIO $ WS.sendTextData conn $ Aeson.encode toSnd
            let getParsedMsg = do
                msgLBS :: LBS.ByteString <- liftIO $ WS.receiveData conn
                let msg = fromMaybe (error $ "FIXME: Cardanode Node message we could not parse!?\n"
                                    <> LBS.unpack msgLBS)
                          $ decode' msgLBS
                logInfo $ pretty $ "Monitor:" <> show thePort <> ": " <> show msg
                pure msg
            -- Wait until we have seen all other nodes connect to this one
            _ <- flip (iterateUntilM (== (numNodes - 1))) 0 $ \n -> do
              msg <- getParsedMsg
              pure $ case msg of
                PeerConnected _ -> n + 1
                _ -> n
            liftIO $ writeChan readyCounterChan ()
            -- Start accepting messages
            forever $ do
              msg <- getParsedMsg
              -- Process state changes and balance changes
              let handleMsg = \case
                    ReadyToCommit {} -> (Just Status_Init, Nothing)
                    Committed {} -> (Just Status_Committing, Nothing)
                    HeadIsOpen utxoz -> (Just Status_Open, Just utxoz)
                    HeadIsClosed {} -> (Just Status_Closed, Nothing)
                    ReadyToFanout {} -> (Just Status_Fanout, Nothing)
                    HeadIsAborted {} -> (Just Status_Aborted, Nothing)
                    HeadIsFinalized utxoz -> (Just Status_Finalized, Just utxoz)
                    _ -> (Nothing, Nothing)
              case handleMsg msg of
                (Just status, mbalance) -> do
                  liftIO $ modifyMVar_ (_state_instances state) $ \iss -> do
                    case _head_status <$> (Map.lookup name . instance_heads <=< Map.lookup key) iss of
                      Just currentStatus ->
                        when (currentStatus /= status && status == Status_Fanout) $ void $ forkIO $ do
                       _ <- fanoutHead state key name
                       pure ()
                      _ -> pure ()
                    atomically $ writeTChan statusBTChan status
                    pure $ Map.adjust (\is -> is { instance_heads = Map.adjust (\h -> h { _head_status = status }) name $ instance_heads is }) key iss

                  subsPerHead <- liftIO $ withMVar (_state_instances state) $ pure . maybe mempty instance_subscribers . Map.lookup key
                  case Map.lookup name subsPerHead of
                    Nothing -> pure ()
                    Just subs -> liftIO $
                      for_ subs $ \subConn -> do
                      mappedBalance <- case mbalance of
                        Just balance ->
                          reverseMapProxyBalances state participants $ wholeUtxoToBalanceMap balance
                        Nothing ->
                          pure $ Right $ Map.fromList $ (,0) <$> Set.toList participants
                      case mappedBalance of
                        Right mb ->
                          WS.sendTextData subConn . Aeson.encode $ HeadStatusChanged name status mb
                        Left errMsg ->
                          withLogging $ logError $ pretty $ "Failed to get head balance in head " <> name <> ": " <> T.pack errMsg
                  pure ()
                (Nothing, _) -> pure ()
              liftIO $ atomically $ writeTChan bRcvChan msg
        pure $ Node
          { _node_handle = processHndl
          , _node_info = nodeInfo
          , _node_monitor_thread = monitor
          , _node_send_msg = writeChan sndChan
          , _node_get_listen_chan = atomically $ dupTChan bRcvChan
          , _node_ports = ports
          }

      -- This is finnicky, essentially we need to make sure the node didn't fail
      -- it may _never_ fail, so we just wait a certain amount of time
      liftIO $ threadDelay 3000000
      logInfo [i|---- Checking integrity of #{numNodes} nodes... |]
      nodeFailure <- fmap (any isJust) $ lift $ flip Map.traverseWithKey (_network_nodes network) $ \addr node -> do
        result <- liftIO $ getProcessExitCode $ _node_handle node
        when (isJust result) $ logError [i|--- Node failed inspect #{nodeLogDir}/hydra-node-#{addressToString addr}.error.log for more information|]
        pure result

      when nodeFailure $ do
        lift $ terminateNetwork state key network
        throwError "Failed to start network, check Hydra Pay logs for more specific information"

      logInfo [i|---- Integrity sound waiting for #{numNodes} nodes to get ready |]
      let countReadies n = do
            if n == numNodes
              then logInfo [i|---- All nodes ready|]
              else do
                liftIO $ readChan readyCounterChan
                logInfo [i|---- #{n + 1} nodes ready of #{numNodes}|]
                countReadies (n + 1)
      countReadies 0

      -- Add the network to the running networks mvar
      liftIO $ modifyMVar_ (_state_instances state) $ \iss -> do
        pure $ Map.adjust (\inst -> inst { instance_networks = Map.insert name network $ instance_networks inst }) key iss
      pure network

-- | This takes the set participants in a Head and gets their proxy equivalents as actual addresses
-- participating in the network are not the addresses registered in the head, but their proxies
participantsToProxyMap :: MonadIO m => State -> Set Address -> m (Either String (Map Address HydraKeyInfo))
participantsToProxyMap state participants = runExceptT $
  fmap Map.fromList $ for (Set.toList participants) $ \x ->
  ExceptT $ liftIO $ addOrGetKeyInfo state x

-- | Lookup the network associated with a head name
getNetwork :: MonadIO m => State -> ApiKey -> HeadName -> m (Maybe Network)
getNetwork state key name =
  liftIO $ withMVar (_state_instances state) (pure . (Map.lookup name  . instance_networks <=< Map.lookup key))

-- | Where the nodes log their output
nodeLogDir :: FilePath
nodeLogDir ="node-logs"

startHydraNetwork :: (MonadIO m)
  => HydraSharedInfo
  -> Map Address HydraKeyInfo
  -> (Int -> IO (Maybe [Int]))
  -> m (Map Address (ProcessHandle, HydraNodeInfo, [Int]))
startHydraNetwork sharedInfo actors getPorts = do
  liftIO $ createDirectoryIfMissing True nodeLogDir
  nodes :: (Map Address (HydraNodeInfo, [Int])) <- liftIO . fmap Map.fromList . mapM node $ zip [1 ..] (Map.toList actors)
  liftIO $ sequence . flip Map.mapWithKey nodes $ \name (theNode, ports) -> do
    logHndl <- openFile [iii|#{nodeLogDir}/hydra-node-#{addressToString name}.log|] WriteMode
    errHndl <- openFile [iii|#{nodeLogDir}/hydra-node-#{addressToString name}.error.log|] WriteMode
    let cp = (mkHydraNodeCP sharedInfo theNode (filter ((/= _nodeId theNode) . _nodeId) (fst <$> Map.elems nodes)))
             { std_out = UseHandle logHndl
             , std_err = UseHandle errHndl
             }
    (_,_,_,resultHandle) <- createProcess cp

    pure (resultHandle, theNode, ports)
  where

    node :: forall a. (Int, (a, HydraKeyInfo)) -> IO (a, (HydraNodeInfo, [Int]))
    node (n, (name, keys)) = do
      pd <- getTempPath'
      Just ps@[p1,p2,p3] <- getPorts 3
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
nodeArgs (HydraNodeInfo nodeId p apiPort monitoringPort
           (HydraKeyInfo
            (KeyPair cskPath _cvkPath)
            (KeyPair hskPath _hvkPath))
           persistenceDir) =
  [ "--node-id"
  , show nodeId
  , "--port"
  , show p
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

-- | Sign and submit a transaction on the Node given, the return is the error if one exists
signAndSubmitTx :: CardanoNodeInfo -> SigningKey -> Tx -> IO (Maybe String)
signAndSubmitTx cninfo sk tx =
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
      (exitCode, _, stderr) <- readCreateProcessWithExitCode cp ""
      case exitCode of
        ExitSuccess -> do
          submitResult <- submitTx cninfo signedFile
          case submitResult of
            Right txid -> do
              withLogging . waitForTxIn cninfo $ txInput 0 txid
              pure Nothing
            Left err -> pure $ Just err
        _ -> pure $ Just stderr

handleTaggedMessage :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => WS.Connection -> T.Text -> State -> Tagged ClientMsg -> m (Tagged ServerMsg)
handleTaggedMessage conn apiKey state (Tagged tid clientMsg) = do
  serverMsg <- handleClientMessage conn apiKey state clientMsg
  pure $ Tagged tid serverMsg

handleClientMessage :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => WS.Connection -> T.Text -> State -> ClientMsg -> m ServerMsg
handleClientMessage conn key state = \case
  GetRestartTime -> do
    t <- liftIO $ readMVar (_state_nextClearTime state)
    pure $ RestartTime t

  Authenticate token -> do
    let apiKeys = _state_apiKeys state
        sentKey = T.encodeUtf8 token

    pure $ AuthResult $ any (sentKey ==) apiKeys

  GetHeadBalance headname addr -> do
    result <- headBalance state key headname addr
    pure $ case result of
      Left err -> ServerError err
      Right balance -> HeadBalance balance

  SubmitHeadTx addr hstx@(HeadSubmitTx hname _ _) -> do
    result <- submitTxOnHead state key addr hstx
    case result of
      Left err -> pure $ ServerError err
      Right pico -> do
        ebs <- headAllBalances state key hname
        case ebs of
          Left _ ->
            logWarning $ "Failed to get Whole UTxO for head : " <> pretty hname <> " even though the a transaction successfully submitted..."
          Right bs ->
            broadcastToSubscribers state key hname $ BalanceChange hname bs
        pure $ TxConfirmed pico

  GetL1Balance addr -> do
    L1Balance <$> l1Balance state addr False

  GetStats -> do
    numHeads <- toInteger . maybe 0 id . fmap (Map.size . instance_heads) . Map.lookup key <$> liftIO (readMVar (_state_instances state))
    networks <- maybe mempty instance_networks . Map.lookup key <$> (liftIO $ readMVar (_state_instances state))

    let
      numNodes :: Integer
      numNodes = toInteger $ Map.foldr (+) 0 . fmap (Map.size . _network_nodes) $ networks

    pure $ CurrentStats $ HydraPayStats numHeads numNodes

  RestartDevnet -> do
    case getHydraPayMode state of
      ConfiguredMode {} -> pure $ ApiError "Hydra Pay is currently not running a Devnet"
      ManagedDevnetMode -> do
        instances <- liftIO $ readMVar (_state_instances state)
        for instances $ \is -> do
          -- NOTE(skylar): Remember terminating the head removes the network!
          for_ (Map.keys $ instance_heads is) $ terminateHead state $ instance_apiKey is

        broadcastToAll state DevnetRestarted

        -- Create a new devnet and update the MVar
        liftIO $ modifyMVar (_state_cardanoNodeState state) $ \oldCns -> do
          teardownDevnet oldCns
          result <- withLogging (makeCardanoNodeStateAndRestartDemoDevnet (getHydraPayMode state))
          case result of
            Right newstate -> pure (newstate, DevnetRestarted)
            Left err -> pure (oldCns, ApiError $ "Failed to restart devnet: " <> T.pack err)

  GetDevnetAddresses count -> do
    mAddrs <- liftIO $ withMVar (_state_instanceAddresses state) (pure . (fmap . fmap) fst . Map.lookup key)
    case fmap (take (fromIntegral count)) mAddrs of
      Just addrs ->
        pure $ DevnetAddresses addrs
      Nothing ->
        pure $
        RequestError
        "Seeding is in progress: please wait for your instance addresses to be created and funded"

  SubscribeTo name -> do
    liftIO $ modifyMVar_ (_state_instances state) $ pure . Map.adjust (\i -> i {instance_subscribers = Map.insertWith (<>) name (pure conn) $ instance_subscribers i}) key
    status <- getHeadStatus state key name
    pure $ either (ApiError . T.pack . show) SubscriptionStarted status

  LiveDocEzSubmitTx tx addr -> do
    addrs <- liftIO $ withMVar (_state_instanceAddresses state) $ pure . maybe mempty id . Map.lookup key
    case lookup addr addrs of
      Nothing -> pure $ ApiError "Address is not a Devnet test address"
      Just (KeyPair sk _) -> do
        cardanoNodeInfo <- getCardanoNodeInfo state
        result <- liftIO $ signAndSubmitTx cardanoNodeInfo sk tx
        pure $ case result of
          Just err -> ApiError $ T.pack err
          Nothing -> OperationSuccess

  ClientHello -> pure $ ServerHello versionStr

  Withdraw addr takeFuel -> do
    -- Withdraw everything minus fees
    result <- withdraw state $ WithdrawRequest addr Nothing takeFuel
    case result of
      Right txid -> do
        logInfo $ "Withdrawal transaction submitted for " <> (pretty $ addressToString addr) <> pretty txid
        pure $ WithdrawSubmitted txid
      Left err-> pure $ ApiError $ tShow err

  GetAddTx txtype addr lovelaceAmount -> do
    result <- buildAddTx txtype state addr lovelaceAmount
    pure $ either (ApiError . T.pack) FundsTx result

  CheckFuel addr -> do
    -- Calc the fuel amount
    fuel <- getProxyFuel state addr
    pure $ either (ApiError . T.pack) FuelAmount fuel

  CreateHead hc@(HeadCreate name _) -> do
    result <- createHead state key hc
    case result of
      Left err -> pure $ ServerError err
      Right _ ->
        either ServerError HeadInfo <$> getHeadStatus state key name

  InitHead hi -> do
    result <- initHead state key hi
    pure $ either ApiError (const OperationSuccess) result

  CommitHead hc -> do
    result <- commitToHead state key hc
    pure $ either ServerError (const OperationSuccess) result

  CloseHead name -> do
    result <- closeHead state key name
    pure $ either ServerError (const OperationSuccess) result

  DoesHeadExist name -> do
    result <- getHeadStatus state name key
    pure $ HeadExistsResult $ case result of
      Left _ -> False
      Right _ -> True

  TearDownHead name -> do
    result <- removeHead state key name
    case result of
      Right () -> do
        let removeMsg = HeadRemoved name
        broadcastToSubscribers state key name removeMsg
        pure removeMsg
      Left err -> do
        pure $ ApiError $ T.pack err

  GetIsManagedDevnet -> pure . IsManagedDevnet . (== ManagedDevnetMode) . getHydraPayMode $ state
  GetHydraPayMode -> pure . HydraPayMode . getHydraPayMode $ state

  GetProxyInfo addr -> do
    inf <- addOrGetKeyInfo state addr
    case inf of
      Right (proxyAddr, _) -> do
        balance <- l1Balance state proxyAddr False
        fuel <- fuelBalance state proxyAddr
        pure $ ProxyAddressInfo $ ProxyInfo
          addr
          proxyAddr
          balance
          fuel
      Left err -> pure $ ServerError (ProcessError err)


newtype HydraPayClient a = HydraPayClient
  { unHydraPayClient :: MaybeT (ReaderT ClientState IO) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadFail
           )

data ClientState = ClientState
  { clientState_connection :: WS.Connection
  , clientState_inbox :: TChan ApiMsg
  , clientState_msgs :: TChan ServerMsg
  , clientState_nextId :: MVar Int64
  }

runHydraPayClient :: BS.ByteString -> HydraPayClient a -> IO (Maybe a)
runHydraPayClient apiKey action = do
  nextId <- newMVar 0
  broadcastChannel <- newBroadcastTChanIO
  msgsChannel <- newBroadcastTChanIO
  WS.runClient "127.0.0.1" 8000 "hydra/api" $ \conn -> do
    -- We have a read thread that is centralized so we don't miss any messages
    _ <- forkIO $ forever $ do
      mMsg :: Maybe ApiMsg <- Aeson.decode <$> WS.receiveData conn
      case mMsg of
        Just (PlainMsg p) ->
          atomically $ writeTChan msgsChannel p
        Just msg ->
          atomically $ writeTChan broadcastChannel msg
        Nothing -> pure ()
      pure ()
    flip runReaderT (ClientState conn broadcastChannel msgsChannel  nextId) $ runMaybeT $ unHydraPayClient $ do
      AuthResult True <- requestResponse $ Authenticate $ T.decodeUtf8 apiKey
      action

requestResponse :: ClientMsg -> HydraPayClient ServerMsg
requestResponse clientMsg = HydraPayClient . MaybeT . ReaderT $ \(ClientState conn inbox _ nid) -> do
  n <- modifyMVar nid $ \x -> pure (x + 1, x)
  readChannel <- atomically $ dupTChan inbox
  WS.sendTextData conn . Aeson.encode $ Tagged n clientMsg
  Just <$> waitForResponse n readChannel
  where
    waitForResponse n chan = do
      msg <- atomically $ readTChan chan
      case msg of
        TaggedMsg (Tagged gotN resultMsg) | n == gotN -> pure resultMsg
        _ -> waitForResponse n chan

waitForHeadOpen :: HeadName -> HydraPayClient ()
waitForHeadOpen hname = HydraPayClient . MaybeT . ReaderT $ \(ClientState _ _ box _) -> do
  readChannel <- atomically $ dupTChan box
  checkForOpenStatus readChannel
  pure (Just ())
  where
    checkForOpenStatus box = do
      msg <- atomically $ readTChan box
      case msg of
        HeadStatusChanged name Status_Open _ | name == hname -> pure ()
        _ -> checkForOpenStatus box
