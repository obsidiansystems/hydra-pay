-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad
import Hydra.Devnet

type HeadId = Int

data ProxyAddressesT f = ProxyAddress
  { proxyAddress_ownerAddress :: Columnar f T.Text
  , proxyAddress_address :: Columnar f T.Text

  , proxyAddress_cardanoPublickKey :: Columnar f T.Text
  , proxyAddress_cardanoPrivateKey :: Columnar f T.Text
  , proxyAddress_hydraPublicKey :: Columnar f T.Text
  , proxyAddress_hydraPrivateKey :: Columnar f T.Text
  }
  deriving (Generic, Beamable)

type ProxyAddress = ProxyAddressesT Identity

data HydraDB f = HydraDB
  { hydraDb_proxyAddresses :: f (TableEntity ProxyAddressesT)
  }
  deriving (Generic, Database be)

instance Table ProxyAddressesT where
  data PrimaryKey ProxyAddressesT f = ProxyAddressID (Columnar f T.Text)
    deriving (Generic, Beamable)
  primaryKey = ProxyAddressID . proxyAddress_ownerAddress

hydraDb :: DatabaseSettings Postgres HydraDB
hydraDb = defaultDbSettings

hydraDbAnnotated :: BA.AnnotatedDatabaseSettings Postgres HydraDB
hydraDbAnnotated = BA.defaultAnnotatedDbSettings hydraDb

hsSchema :: BA.Schema
hsSchema = BA.fromAnnotatedDbSettings hydraDbAnnotated (Proxy @'[])

-- withDb :: String -> (Connection -> IO a) -> IO a
-- withDb dbPath a = withGargoyle defaultPostgres dbPath $ \dbUri -> a =<< connectPostgreSQL dbUri

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
  { headCreate_participants :: [Address]
  }
  deriving (Generic)

instance ToJSON HeadCreate
instance FromJSON HeadCreate


-- TODO(skylar): Ya we def want a monad
withLogging = flip runLoggingT (print . renderWithSeverity id)

createHead :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => Pool Connection -> HeadCreate -> m HeadId
createHead pool (HeadCreate participants) = do
  -- Create proxy addresses for all the addresses that don't already have one
  liftIO $ withResource pool $ \conn -> do
    for_ participants $ \addr -> do
      exists <- proxyAddressExists conn addr
      when (not exists) $ withLogging $ addProxyAddress addr conn
  -- Create the node structure to be able to run the thing
  pure 1

standupDemoHydraNetwork' :: (MonadIO m)
  => HydraScriptTxId
  -> Map Address HydraKeyInfo
  -> m (Map T.Text (ProcessHandle, HydraNodeInfo))
standupDemoHydraNetwork' hstxid actors = do
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
