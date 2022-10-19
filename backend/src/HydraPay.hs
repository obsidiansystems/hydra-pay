-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}


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
import qualified Data.Text as T
import Hydra.Types
import Data.Int
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Aeson
import Data.Proxy
import Data.Pool
import Data.Maybe
import Database.Beam
import Database.Beam.Postgres
import qualified Database.Beam.AutoMigrate as BA
import Data.String.Interpolate ( i, iii )
import System.IO (IOMode(WriteMode), openFile)

import Control.Concurrent

import Data.Text.Prettyprint.Doc
import Control.Monad.Log
import System.Directory

import Database.PostgreSQL.Simple as Pg
import Gargoyle
import Gargoyle.PostgreSQL (defaultPostgres)
import Gargoyle.PostgreSQL.Connect

import Data.Foldable
import Data.Traversable

import Control.Monad
import Hydra.Devnet

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

-- NOTE TODO(skylar): So beam doesn't have many to many or uniqueness constraints
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

-- TODO(skylar): Put this in a monad?
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

addProxyAddress :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => Address -> Connection -> m ()
addProxyAddress addr conn = do
  path <- liftIO getKeyPath
  keyInfo <- generateKeysIn path

  let
    cvk = _verificationKey . _cardanoKeys $ keyInfo
    csk = _signingKey . _cardanoKeys $ keyInfo
    hvk = _verificationKey . _hydraKeys $ keyInfo
    hsk = _signingKey . _hydraKeys $ keyInfo

  cardanoAddress <- liftIO $ getCardanoAddress $ _verificationKey . _cardanoKeys $ keyInfo

  liftIO $ runBeamPostgres conn $ runInsert $ insert (hydraDb_proxyAddresses hydraDb) $
    insertValues [ProxyAddress addr cardanoAddress (T.pack cvk) (T.pack csk) (T.pack hvk) (T.pack hsk)]
  pure ()

-- | The location where we store cardano and hydra keys
getKeyPath :: IO (FilePath)
getKeyPath = do
  createDirectoryIfMissing True path
  pure path
  where
    path = "keys"

-- TODO(skylar): Haddocks for this
data HeadCreate = HeadCreate
  { headCreate_name :: T.Text -- TODO(skylar): This needs to be a type for size reasons most likely
  -- TODO(skylar): If we make participants a set, the JSON gets harder to write but we should probably make the types as nice as we can internally for those magic strong guarnatees
  , headCreate_participants :: [Address]
  , headCreate_startNetwork :: Bool
  }
  deriving (Generic)

instance ToJSON HeadCreate
instance FromJSON HeadCreate

-- TODO(skylar): Ya we def want a monad
withLogging = flip runLoggingT (print . renderWithSeverity id)

-- This is the API json type that we need to send back out
data HeadStatus = HeadStatus
  { headStatus_name :: T.Text
  -- TODO(skylar): The running should become like a "Network State" likely
  , headStatus_running :: Bool
  , headStatus_status :: Status
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStatus
instance FromJSON HeadStatus

-- TODO(skylar): How friendly can we make the errors?
data HydraPayError
  = InvalidPayload
  | HeadCreationFailed
  | NotEnoughParticipants
  | HeadExists HeadName
  | HeadDoesn'tExist
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

createHead' :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => Pool Connection -> HeadCreate -> m (Either HydraPayError (HeadStatus, HydraHeadNetwork))
createHead' pool (HeadCreate name participants _) = do
  -- Create proxy addresses for all the addresses that don't already have one
  liftIO $ withResource pool $ \conn -> do
    for_ participants $ \addr -> do
      exists <- proxyAddressExists conn addr
      when (not exists) $ withLogging $ addProxyAddress addr conn
    -- We need the script id
    -- We need to insert the head into the db then start the Head
    runBeamPostgres conn $ do
      -- Create the head
      runInsert $ insert (hydraDb_heads hydraDb) $
        insertValues [ DbHead name initialStatus ]

      -- Populate the participant list
      runInsert $ insert (hydraDb_headParticipants hydraDb) $
        insertExpressions $ fmap (\addr -> HeadParticipant (val_ $ HeadID name) (val_ $ ProxyAddressID addr)) participants

    mKeyinfos <- fmap sequenceA $ for participants (\a -> fmap (a,) <$> getProxyAddressKeyInfo conn a)
    case mKeyinfos of
      Nothing -> pure $ Left HeadCreationFailed
      Just keyinfos -> do
        scripts <- withLogging $ publishReferenceScripts
        network <- startHydraNetwork scripts $ Map.fromList keyinfos
        -- TODO(skylar): This is always true here, but should it be? aka how to represent network failure in startup
        pure $ Right (HeadStatus name True Pending, network)
  where
    initialStatus = "Pending"

-- TODO(skylar): How to bundle this with the rest of the types
type HydraHeadNetwork = (Map Address (ProcessHandle, HydraNodeInfo))

getProxyAddressKeyInfo :: MonadIO m => Connection -> Address -> m (Maybe HydraKeyInfo)
getProxyAddressKeyInfo conn addr = liftIO $ do
  mpa <- runBeamPostgres conn $ runSelectReturningOne $ select $ do
    pa <- all_ (hydraDb_proxyAddresses hydraDb)
    guard_ $ proxyAddress_ownerAddress pa ==. val_ addr
    pure pa
  pure $ dbProxyToHydraKeyInfo <$> mpa

-- TODO(skylar): Should this include the ProxyAddress and the Address and should we
-- differentiate them at type level?
dbProxyToHydraKeyInfo :: ProxyAddress -> HydraKeyInfo
dbProxyToHydraKeyInfo pa = keyInfo
  -- (proxyAddress_proxyAddress, pa keyInfo)
  where
    keyInfo =
      HydraKeyInfo
      (KeyPair (T.unpack $ proxyAddress_cardanoSigningKey pa) (T.unpack $ proxyAddress_cardanoVerificationKey pa))
      (KeyPair (T.unpack $ proxyAddress_hydraSigningKey pa) (T.unpack $ proxyAddress_hydraVerificationKey pa))

-- addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr
-- addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g

-- HeadCreate "test" ["addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr", "addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g"]

-- "{\"headCreate_name\":\"test\",\"headCreate_participants\":[\"addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr\",\"addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g\"]}"


type HeadName = T.Text

-- | State we need to run/manage Heads
data State = State
  { _state_scripts :: HydraScriptTxId
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
  = Pending
  | Init
  | Commiting
  | Open
  | Closed
  | Fanout
  deriving (Eq, Show, Generic)

instance ToJSON Status
instance FromJSON Status

-- | A Hydra Node running as part of a network
data Node = Node
  { _node_handle :: ProcessHandle
  , _node_info :: HydraNodeInfo
  }

-- | The network of nodes that hold up a head
data Network = Network
  { _network_nodes :: Map Address Node
  }

getHydraPayState :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m State
getHydraPayState = do
  scripts <- publishReferenceScripts
  addrs <- liftIO $ newMVar mempty
  heads <- liftIO $ newMVar mempty
  networks <- liftIO $ newMVar mempty
  path <- liftIO $ getKeyPath
  pure $ State scripts addrs heads networks path

-- TODO(skylar): MonadThrow here?
-- NOTE(skylar): I don't think creating a head should be idempotent that would be weird
createHead :: MonadIO m => State -> HeadCreate -> m (Either HydraPayError Head)
createHead state (HeadCreate name participants start) = do
  -- TODO(skylar): This check can be done better by converting to a NonEmpty list and checking failure
  -- rather than just checking the length
  case null participants of
    True -> pure $ Left NotEnoughParticipants
    False -> do
      mHead <- lookupHead state name
      case mHead of
        Just _ -> pure $ Left $ HeadExists name
        Nothing -> do
          let head = Head name (Set.fromList participants) Pending
          liftIO $ modifyMVar_ (_state_heads state) $ pure . Map.insert name head
          pure $ Right head

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
        keyInfo <- withLogging $ generateKeysIn path
        proxyAddress <- liftIO $ getCardanoAddress $ _verificationKey . _cardanoKeys $ keyInfo
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
      network <- fmap (Network . fmap (uncurry Node)) $ startHydraNetwork (_state_scripts state) proxyMap

      -- Add the network to the running networks mvar
      liftIO $ modifyMVar_ (_state_networks state) $ pure . Map.insert name network

      pure network

-- TODO(skylar): Naming and should this take a Head or the Set Address?/
-- | This takes the set participants in a Head and gets their proxy equivalents as actual addresses
-- participating in the network are not the addresses registered in the head, but their proxies
participantsToProxyMap :: MonadIO m => State -> Set Address -> m (Map Address HydraKeyInfo)
participantsToProxyMap state participants = liftIO $ fmap Map.fromList $ for (Set.toList participants) $ addOrGetKeyInfo state

-- | Lookup the network associated with a head name
getNetwork :: MonadIO m => State -> HeadName -> m (Maybe Network)
getNetwork state name =
  liftIO $ withMVar (_state_networks state) (pure . Map.lookup name)

-- TODO(skylar): Make this take state and then make this return a Network
-- TODO(skylar): Port management
-- TODO(skylar): Where should the HydraScriptTxId come from?
startHydraNetwork :: (MonadIO m)
  => HydraScriptTxId
  -> Map Address HydraKeyInfo
  -> m (Map Address (ProcessHandle, HydraNodeInfo))
startHydraNetwork hstxid actors = do
  -- TODO(skylar): These logs should be Head specific
  liftIO $ createDirectoryIfMissing True "demo-logs"
  liftIO $ sequence . flip Map.mapWithKey nodes $ \name node -> do
    logHndl <- openFile [iii|demo-logs/hydra-node-#{name}.log|] WriteMode
    errHndl <- openFile [iii|demo-logs/phydra-node-#{name}.error.log|] WriteMode
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
    sharedInfo = HydraSharedInfo
      { _hydraScriptsTxId = T.unpack hstxid
      , _ledgerGenesis = "devnet/genesis-shelley.json"
      , _ledgerProtocolParameters = "devnet/protocol-parameters.json"
      , _networkId = show devnetMagic
      , _nodeSocket = "devnet/node.socket"
      }

data HydraSharedInfo = HydraSharedInfo
  { _hydraScriptsTxId :: String
  , _ledgerGenesis :: FilePath
  , _ledgerProtocolParameters :: FilePath
  , _networkId :: String
  , _nodeSocket :: FilePath
  }

data HydraNodeInfo = HydraNodeInfo
  { _nodeId :: Int
  , _port :: Int
  , _apiPort :: Int
  , _monitoringPort :: Int
  , _keys :: HydraKeyInfo
  }

-- | Takes the node participant and the list of peers
mkHydraNodeCP :: HydraSharedInfo -> HydraNodeInfo -> [HydraNodeInfo] -> CreateProcess
mkHydraNodeCP sharedInfo node peers =
  (proc hydraNodePath $ sharedArgs sharedInfo <> nodeArgs node <> concatMap peerArgs peers)
  { std_out = Inherit
  }

sharedArgs :: HydraSharedInfo -> [String]
sharedArgs (HydraSharedInfo hydraScriptsTxId ledgerGenesis protocolParams networkId nodeSocket) =
  [ "--ledger-genesis"
  , ledgerGenesis
  , "--ledger-protocol-parameters"
  , protocolParams
  , "--network-id"
  , networkId
  , "--node-socket"
  , nodeSocket
  , "--hydra-scripts-tx-id"
  , hydraScriptsTxId
  ]

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
  , hskPath
  , "--cardano-signing-key"
  , cskPath
  ]

peerArgs :: HydraNodeInfo -> [String]
peerArgs ni =
  [ "--peer"
  , [i|127.0.0.1:#{_port ni}|]
  , "--hydra-verification-key"
  , _verificationKey . _hydraKeys . _keys $ ni
  , "--cardano-verification-key"
  , _verificationKey . _cardanoKeys . _keys $ ni
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
