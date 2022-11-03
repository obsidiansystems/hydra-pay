-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HydraPay where

{-
What is the path of head creation?
Init requires funds, and it requires that people

In simple terms we just need a list of participants
because we need to setup the nodes, then we need to init as one of the participants

The issue here is each participant needs fuel, and each participant needs a cardano key
and a hydra key.

As part of our usecase we can't have people entering their seed phrases. Which means
until we have external de/commit, we must use proxy addresses that the user can send funds to
this means that we likely have to provide endpoints to make this convenient!

So in essence creating a head is providing a list of addresses and participants
These addresses and participants will need to have fuel and fund things, and this

What is a Head?
A collection of nodes that may have state on L1
HeadId exists on-chain

Creating a head means sending the participant list

The backend needs to prepare translated addresses for each participant

State Management:

-}
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
import Data.Proxy
import Data.Pool
import Data.Maybe
import Database.Beam
import Database.Beam.Postgres
import qualified Database.Beam.AutoMigrate as BA
import Data.String.Interpolate ( i, iii )
import System.IO (IOMode(WriteMode), openFile)
import Network.WebSockets.Client
import qualified Network.WebSockets.Connection as WS

import Control.Concurrent

import Data.Text.Prettyprint.Doc
import Control.Monad.Log
import System.Directory

import Gargoyle.PostgreSQL.Connect

import Data.Traversable

import Control.Monad

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

type HeadId = Int

data ProxyAddressesT f = ProxyAddress
  { proxyAddress_ownerAddress :: Columnar f T.Text
  , proxyAddress_address :: Columnar f T.Text

  , proxyAddress_cardanoVerificationKey :: Columnar f T.Text
  , proxyAddress_cardanoSigningKey :: Columnar f T.Text
  , proxyAddress_hydraVerificationKey :: Columnar f T.Text
  , proxyAddress_hydraSigningKey :: Columnar f T.Text
  }
  deriving (Generic, Beamable)

data HeadsT f = DbHead
  { head_name :: Columnar f T.Text
  , head_state :: Columnar f T.Text
  }
  deriving (Generic, Beamable)

-- NOTE So beam doesn't have many to many or uniqueness constraints
-- we would have to use beam-migrate or beam-automigrate to include these things as
-- they are properites of the database.
data HeadParticipantsT f = HeadParticipant
  { headParticipant_head :: PrimaryKey HeadsT f
  , headParticipant_proxy :: PrimaryKey ProxyAddressesT f
  }
  deriving (Generic, Beamable)

type ProxyAddress = ProxyAddressesT Identity

data HydraDB f = HydraDB
  { hydraDb_proxyAddresses :: f (TableEntity ProxyAddressesT)
  , hydraDb_heads :: f (TableEntity HeadsT)
  , hydraDb_headParticipants :: f (TableEntity HeadParticipantsT)
  }
  deriving (Generic, Database be)

instance Table ProxyAddressesT where
  data PrimaryKey ProxyAddressesT f = ProxyAddressID (Columnar f T.Text)
    deriving (Generic, Beamable)
  primaryKey = ProxyAddressID . proxyAddress_ownerAddress

instance Table HeadsT where
  data PrimaryKey HeadsT f = HeadID (Columnar f T.Text)
    deriving (Generic, Beamable)
  primaryKey = HeadID . head_name

instance Table HeadParticipantsT where
  data PrimaryKey HeadParticipantsT f = HeadParticipantID (PrimaryKey HeadsT f) (PrimaryKey ProxyAddressesT f)
    deriving (Generic, Beamable)
  primaryKey = HeadParticipantID <$> headParticipant_head <*> headParticipant_proxy

hydraDb :: DatabaseSettings Postgres HydraDB
hydraDb = defaultDbSettings

hydraDbAnnotated :: BA.AnnotatedDatabaseSettings Postgres HydraDB
hydraDbAnnotated = BA.defaultAnnotatedDbSettings hydraDb

hsSchema :: BA.Schema
hsSchema = BA.fromAnnotatedDbSettings hydraDbAnnotated (Proxy @'[])

hydraShowMigration :: Connection -> IO ()
hydraShowMigration conn =
  runBeamPostgres conn $ BA.printMigration $ BA.migrate conn hsSchema

hydraAutoMigrate :: Connection -> IO ()
hydraAutoMigrate = BA.tryRunMigrationsWithEditUpdate hydraDbAnnotated

withHydraPool :: (Pool Connection -> IO a) -> IO a
withHydraPool action = withDb "db" $ \pool -> do
  withResource pool $ \conn -> do
    hydraShowMigration conn
    hydraAutoMigrate conn
  action pool

proxyAddressExists :: Connection -> Address -> IO Bool
proxyAddressExists conn addr = do
  fmap isJust $ runBeamPostgres conn $ runSelectReturningOne $ select $ do
    pa <- all_ (hydraDb_proxyAddresses hydraDb)
    guard_ $ proxyAddress_ownerAddress pa ==. val_ addr
    pure pa

addProxyAddress :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> Address -> Connection -> m ()
addProxyAddress cninf addr conn = do
  path <- liftIO getKeyPath
  keyInfo <- generateKeysIn $ T.unpack addr

  let
    cvk = getVerificationKeyFilePath $ _verificationKey . _cardanoKeys $ keyInfo
    csk = getSigningKeyFilePath $ _signingKey . _cardanoKeys $ keyInfo
    hvk = getVerificationKeyFilePath $ _verificationKey . _hydraKeys $ keyInfo
    hsk = getSigningKeyFilePath $ _signingKey . _hydraKeys $ keyInfo

  cardanoAddress <- liftIO $ getCardanoAddress cninf $ _verificationKey . _cardanoKeys $ keyInfo

  liftIO $ runBeamPostgres conn $ runInsert $ insert (hydraDb_proxyAddresses hydraDb) $
    insertValues [ProxyAddress addr cardanoAddress (T.pack cvk) (T.pack csk) (T.pack hvk) (T.pack hsk)]
  pure ()

-- | The location where we store cardano and hydra keys
getKeyPath :: IO FilePath
getKeyPath = do
  createDirectoryIfMissing True path
  pure path
  where
    path = "keys"

-- TODO: All these action which are about a named head repeat this
-- fact. Use structure to aid code reuse and help to see patterns.

data HeadCreate = HeadCreate
  { headCreate_name :: T.Text
  , headCreate_participants :: [Address]
  , headCreate_startNetwork :: Bool
  }
  deriving (Generic)

instance ToJSON HeadCreate
instance FromJSON HeadCreate

data HeadInit = HeadInit
  { headInit_name :: T.Text
  , headInit_participant :: Address
  , headInit_contestation :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadInit
instance FromJSON HeadInit

data HeadCommit = HeadCommit
  { headCommit_name :: T.Text
  , headCommit_participant :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadCommit
instance FromJSON HeadCommit

data HeadSubmitTx = HeadSubmitTx
  { headSubmitTx_name :: T.Text
  , headSubmitTx_toAddr :: Address
  , amount :: Lovelace
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadSubmitTx
instance FromJSON HeadSubmitTx

withLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
withLogging = flip runLoggingT (print . renderWithSeverity id)

-- This is the API json type that we need to send back out
data HeadStatus = HeadStatus
  { headStatus_name :: T.Text
  , headStatus_running :: Bool
  , headStatus_status :: Status
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStatus
instance FromJSON HeadStatus

data HydraPayError
  = InvalidPayload
  | HeadCreationFailed
  | NotEnoughParticipants
  | HeadExists HeadName
  | HeadDoesn'tExist
  | NetworkIsn'tRunning
  | FailedToBuildFundsTx
  | NotAParticipant
  | InsufficientFunds
  | FanoutNotPossible
  | TxInvalid {utxo :: WholeUTXO, transaction :: Value, validationError :: ValidationError}
  deriving (Eq, Show, Generic)

instance ToJSON HydraPayError
instance FromJSON HydraPayError

{-
What does it mean to get the head status?

Check if the network is running
Fetch status as last seen from the database
Scan the logs to actually have the network state
-}

{-
What does it mean to create a head?
Really just to list the participants and record that as a head that may exist
the Head could also be called created when it is actually initialized on L1
so we should have the different parts of getting to the init state

So creation in HydraPay terms is simply the intent to actually have one of these heads
To get to init a couple of things need to happen:

- Funds need to be sent to the proxy addresses: this constitutes an endpoint for getting the transaction
that will do this?

- Then we have to ensure everybody has Fuel outputs so that is likely another endpoint thing or maybe
they can be combined

- After that somebody just has to send in "Init" which is one of the valid operations you can do to your node
that request should be routed automatically

- Starting and stopping heads should be more or less automatic, we shouldn't worry about that for now
for now creating a head is analogous to running one, but we should be able to run a head by just
-}

getProxyAddressKeyInfo :: MonadIO m => Connection -> Address -> m (Maybe HydraKeyInfo)
getProxyAddressKeyInfo conn addr = liftIO $ do
  mpa <- runBeamPostgres conn $ runSelectReturningOne $ select $ do
    pa <- all_ (hydraDb_proxyAddresses hydraDb)
    guard_ $ proxyAddress_ownerAddress pa ==. val_ addr
    pure pa
  pure $ dbProxyToHydraKeyInfo <$> mpa


dbProxyToHydraKeyInfo :: ProxyAddress -> HydraKeyInfo
dbProxyToHydraKeyInfo pa = keyInfo
  where
    keyInfo =
      HydraKeyInfo
      (mkKeyPair (T.unpack $ proxyAddress_cardanoSigningKey pa) (T.unpack $ proxyAddress_cardanoVerificationKey pa))
      (mkKeyPair (T.unpack $ proxyAddress_hydraSigningKey pa) (T.unpack $ proxyAddress_hydraVerificationKey pa))

type HeadName = T.Text

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
  -- ^ The participants list with proxy addresses and not owner addresses
  }

data Status
  = Status_Pending
  | Status_Init
  | Status_Committing
  | Status_Open
  | Status_Closed
  | Status_Fanout
  | Status_Finalized
  deriving (Eq, Show, Generic)

instance ToJSON Status
instance FromJSON Status

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
  deriving (Eq, Show)

isFuelType :: TxType -> Bool
isFuelType Fuel = True
isFuelType _ = False


headBalance :: (MonadIO m) => State -> T.Text -> Address -> m (Either HydraPayError Lovelace)
headBalance state name addr = do
  withNode state name addr $ \node proxyAddr -> do
    utxos <- getNodeUtxos node proxyAddr
    let
      txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
      fullAmount = sum txInAmounts
    pure (Right fullAmount)


l1Balance :: (MonadIO m) => State -> Address -> m Lovelace
l1Balance state addr = do
  utxos <- queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) addr
  let
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) utxos
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
initHead state (HeadInit name addr con) = do
  withNode state name addr $ \node _ -> do
    liftIO $ _node_send_msg node $ Init $ fromIntegral con
    -- TODO: Response to Init message?
    pure $ Right ()

data WithdrawRequest = WithdrawRequest
  { withdraw_address :: Address
  , withdraw_amount :: Lovelace
  }
  deriving(Eq, Show, Generic)

instance ToJSON WithdrawRequest
instance FromJSON WithdrawRequest


-- | Withdraw funds (if available from proxy request)
withdraw :: MonadIO m => State -> WithdrawRequest -> m (Either HydraPayError TxId)
withdraw state (WithdrawRequest addr lovelace) = do
  (proxyAddr, keyInfo) <- addOrGetKeyInfo state addr
  utxos <- queryAddressUTXOs nodeInfo proxyAddr
  let
    notFuel = filterOutFuel utxos
    txInAmounts = Map.mapMaybe (Map.lookup "lovelace" . HT.value) notFuel
    total = sum txInAmounts

  case total >= lovelace of
    False -> do
      pure $ Left InsufficientFunds

    True -> liftIO $ do
      (filepath, txid) <- buildSignedTx nodeInfo (_signingKey $ _cardanoKeys keyInfo) proxyAddr addr txInAmounts lovelace
      submitTx nodeInfo $ filepath
      pure $ Right txid

  where
    nodeInfo = _cardanoNodeInfo . _state_hydraInfo $ state

fanoutHead :: MonadIO m => State -> T.Text -> m (Either HydraPayError HeadStatus)
fanoutHead = sendToHeadAndWaitFor Fanout $ \case
  HeadIsFinalized {} -> True
  _ -> False

closeHead :: MonadIO m => State -> T.Text -> m (Either HydraPayError HeadStatus)
closeHead = sendToHeadAndWaitFor Close $ \case
  HeadIsClosed {} -> True
  _ -> False

sendToHeadAndWaitFor :: MonadIO m => ClientInput -> (ServerOutput Value -> Bool) -> State -> T.Text -> m (Either HydraPayError HeadStatus)
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
commitToHead state (HeadCommit name addr) = do
  withNode state name addr $ \node proxyAddr -> do
    proxyFunds <- filterOutFuel <$> queryAddressUTXOs (_cardanoNodeInfo . _state_hydraInfo $ state) proxyAddr
    liftIO $ _node_send_msg node $ Commit proxyFunds
    pure $ Right ()

createHead :: MonadIO m => State -> HeadCreate -> m (Either HydraPayError Head)
createHead state (HeadCreate name participants start) = do
  case null participants of
    True -> pure $ Left NotEnoughParticipants
    False -> do
      mHead <- lookupHead state name
      case mHead of
        Just _ -> pure $ Left $ HeadExists name
        Nothing -> do
          let head = Head name (Set.fromList participants) Status_Pending
          liftIO $ for participants $ putStrLn . T.unpack
          liftIO $ modifyMVar_ (_state_heads state) $ pure . Map.insert name head
          when start $ void $ startNetwork state head
          pure $ Right head

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


submitTxOnHead :: (MonadIO m) => State -> Address -> HeadSubmitTx -> m (Either HydraPayError ())
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
    liftIO $ _node_send_msg node $ NewTx . T.pack $ txCborHexStr
    -- TODO: Could we make sure we saw the transaction that we sent and not another one?
    untilJust $ do
      x <- liftIO . atomically $ readTChan c
      case x of
        TxValid tx -> pure . Just $ Right ()
        ServerOutput.TxInvalid utxo tx validationError -> pure . Just $ (Left (HydraPay.TxInvalid utxo tx validationError))
        _ -> pure Nothing



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
    Just (Head name _ status) -> do
      running <- isJust <$> getNetwork state name
      pure $ Right $ HeadStatus name running status

-- | Start a network for a given Head, trying to start a network that already exists is a no-op and you will just get the existing network
startNetwork :: MonadIO m => State -> Head -> m Network
startNetwork state (Head name participants _) = do
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
  , _ledgerGenesis :: FilePath
  , _ledgerProtocolParameters :: FilePath
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
sharedArgs (HydraSharedInfo hydraScriptsTxId ledgerGenesis protocolParams cardanoNodeInfo) =
  [ "--ledger-genesis"
  , ledgerGenesis
  , "--ledger-protocol-parameters"
  , protocolParams
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
