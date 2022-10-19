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
-}
import Prelude hiding ((.))
import Control.Category ((.))
import System.Process
import GHC.Generics
import qualified Data.Text as T
import Hydra.Types
import Data.Int
import Data.Map (Map)
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

data HeadsT f = Head
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
  , headCreate_participants :: [Address]
  }
  deriving (Generic)

instance ToJSON HeadCreate
instance FromJSON HeadCreate

-- TODO(skylar): Ya we def want a monad
withLogging = flip runLoggingT (print . renderWithSeverity id)

-- This is the API json type that we need to send back out
data HeadStatus = HeadStatus
  { headStatus_name :: T.Text
  , headStatus_running :: Bool
  , headStatus_status :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStatus
instance FromJSON HeadStatus

-- TODO(skylar): How friendly can we make the errors?
data HydraPayError
  = InvalidPayload
  | HeadCreationFailed
  deriving (Eq, Show, Generic)

instance ToJSON HydraPayError
instance FromJSON HydraPayError

createHead :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => Pool Connection -> HeadCreate -> m (Either HydraPayError (HeadStatus, HydraHeadNetwork))
createHead pool (HeadCreate name participants) = do
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
        insertValues [ Head name initialStatus ]

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
        pure $ Right (HeadStatus name True initialStatus, network)
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

dbProxyToHydraKeyInfo :: ProxyAddress -> HydraKeyInfo
dbProxyToHydraKeyInfo pa = HydraKeyInfo
  (KeyPair (T.unpack $ proxyAddress_cardanoSigningKey pa) (T.unpack $ proxyAddress_cardanoVerificationKey pa))
  (KeyPair (T.unpack $ proxyAddress_hydraSigningKey pa) (T.unpack $ proxyAddress_hydraVerificationKey pa))

-- addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr
-- addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g

-- HeadCreate "test" ["addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr", "addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g"]

-- "{\"headCreate_name\":\"test\",\"headCreate_participants\":[\"addr_test1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr\",\"addr_test1v8m0dvk84c7hg6rzul6h9f4998vtaqqrupw778vrw3e4ezqck7n2g\"]}"

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
