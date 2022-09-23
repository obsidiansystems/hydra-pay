{-# LANGUAGE TemplateHaskell #-}

module Backend where

import Prelude hiding (filter)

import Hydra.Devnet

import Common.Route
import Common.Api
import Control.Monad
import Obelisk.Backend
import Obelisk.Route

import System.Directory
import Control.Applicative (Alternative)
import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Witherable
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import Control.Concurrent
import System.Process
import System.Which

import Data.Aeson as Aeson

import Data.Some

import Network.WebSockets
import Network.WebSockets.Snap

import Reflex.Dom.GadtApi.WebSocket

import qualified Network.WebSockets as WS
import Debug.Trace

hydraToolsPath :: FilePath
hydraToolsPath = $(staticWhich "hydra-tools")

realpathPath :: FilePath
realpathPath = $(staticWhich "realpath")

dirnamePath :: FilePath
dirnamePath = $(staticWhich "dirname")

queryUTXOs :: MonadIO m => m (T.Text)
queryUTXOs = liftIO $
  T.pack <$> readCreateProcess queryProc ""
  where
    queryProc =
      (proc cardanoCliPath [ "query"
                           , "utxo"
                           , "--whole-utxo"
                           , "--testnet-magic"
                           , "42"
                           ])
      { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

prepareDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ()
prepareDevnet = do
  output <- liftIO $ readCreateProcess (shell "[ -d devnet ] || ./demo/prepare-devnet.sh") ""
  when (null output) $ logMessage $ WithSeverity Informational $ pretty $ T.pack output
  pure ()

standupDemoHydraNetwork :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => HydraScriptTxId -> m (Map String ProcessHandle)
standupDemoHydraNetwork hstxid = do
  result <- liftIO $ mapM createProcessGiveHandle $ (\n -> mkHydraNodeCP hstxid sharedInfo n (filter ((/= _nodeId n) . _nodeId) (Map.elems nodes))) <$> nodes
  logMessage $ WithSeverity Informational "Hydra Network Running for peers Alice, Bob, and Carol"
  pure result
  where
    portNum p n = p * 1000 + n
    node n name =
      ( name
      , HydraNodeInfo n (portNum 5 n) (portNum 9 n) (portNum 6 n)
        (KeyPair [i|devnet/credentials/#{name}.sk|] [i|devnet/credentials/#{name}.vk|])
        (KeyPair [i|#{name}.sk|] [i|#{name}.vk|])
      )
    nodes = Map.fromList [ node 1 "alice", node 2 "bob", node 3 "carol" ]
    sharedInfo = HydraSharedInfo
      { _hydraScriptsTxId = "5f15831e663dd545cdb746a29c174e31544413b1cdb755245549dcf7bfc26485"
      , _ledgerGenesis = "devnet/genesis-shelley.json"
      , _ledgerProtocolParameters = "devnet/protocol-parameters.json"
      , _networkId = "42"
      , _nodeSocket = "devnet/node.socket"
      }
    createProcessGiveHandle cp = do
      (_,_,_,handle) <- createProcess cp
      pure handle

-- | Given the names of the participants generate the CreateProcess for all hydra-nodes
generateHydraNetworkCPs :: HydraScriptTxId -> HydraSharedInfo -> [HydraNodeInfo] -> [CreateProcess]
generateHydraNetworkCPs hstxid sharedInfo nodes =
  map (\n -> mkHydraNodeCP hstxid sharedInfo n (filter ((/= _nodeId n) . _nodeId) nodes)) nodes

-- TODO(skylar): What about failure??
-- We can either fail by having the file be not found, or when we can't parse
-- for the parse part we should use attoparsec or something...
readEnv :: MonadIO m => FilePath -> m [(String, String)]
readEnv envFilePath = liftIO $ parseEnvVars <$> readFile envFilePath
  where
    parseEnvVars = fmap (\x -> (parseVarName x, parseVarVal x)) . lines

    parseVarName = takeWhile (/= '=')
    parseVarVal = drop 1 . dropWhile (/= '=')

-- | Takes the node participant and the list of peers
mkHydraNodeCP :: HydraScriptTxId -> HydraSharedInfo -> HydraNodeInfo -> [HydraNodeInfo] -> CreateProcess
mkHydraNodeCP hstxid sharedInfo node peers =
  traceShow (sharedArgs sharedInfo <> nodeArgs node <> concatMap peerArgs peers) ((proc hydraNodePath $ sharedArgs sharedInfo <> nodeArgs node <> concatMap peerArgs peers)
  { std_out = Inherit
  , env = Just [("HYDRA_SCRIPTS_TX_ID", T.unpack hstxid)]
  })

data KeyPair = KeyPair
  { _signingKey :: String
  , _verificationKey :: String
  }

-- | Generate Cardano keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.cardano.{vk,sk}" keys being
-- written.
generateCardanoKeys :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => String -> m KeyPair
generateCardanoKeys path = do
  output <- liftIO $
    readCreateProcess
    (proc cardanoCliPath [ "address"
                         , "key-gen"
                         , "--verification-key-file"
                         , [i|#{path}.cardano.vk|]
                         , "--signing-key-file"
                         , [i|#{path}.cardano.sk|]
                         ])
    ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  pure $ KeyPair [i|#{path}.cardano.sk|] [i|#{path}.cardano.vk|]


-- | Generate Hydra keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.hydra.{vk,sk}" keys being
-- written.
generateHydraKeys :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => String -> m KeyPair
generateHydraKeys path = do
  output <- liftIO $
    readCreateProcess
    (proc hydraToolsPath [ "gen-hydra-key"
                         , "--output-file"
                         , [i|#{path}.hydra|]
                         ])
    ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  pure $ KeyPair [i|#{path}.hydra.sk|] [i|#{path}.hydra.vk|]

data HydraSharedInfo = HydraSharedInfo
  { _hydraScriptsTxId :: String
  , _ledgerGenesis :: String -- TODO: Path
  , _ledgerProtocolParameters :: String -- TODO: Path
  , _networkId :: String
  , _nodeSocket :: String -- TODO: Path
  }

data HydraNodeInfo = HydraNodeInfo
  { _nodeId :: Int
  , _port :: Int
  , _apiPort :: Int
  , _monitoringPort :: Int
  , _cardanoKeys :: KeyPair
  , _hydraKeys :: KeyPair
  }

-- | Max 1000 nodes.
sequentialNodes :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m, Alternative m) => String -> Int -> m [HydraNodeInfo]
sequentialNodes prefix numNodes = do
  guard (numNodes < 1000)
  forM [1 .. numNodes] $ \n -> do
    let portNum p = p * 1000 + n
    cks <- generateCardanoKeys [i|#{prefix}#{n}|]
    hks <- generateHydraKeys [i|#{prefix}#{n}|]
    pure $ HydraNodeInfo
           { _nodeId = n
           , _port = portNum 5
           , _apiPort = portNum 9
           , _monitoringPort = portNum 6
           , _cardanoKeys = cks
           , _hydraKeys = hks
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
           (KeyPair cskPath _cvkPath)
           (KeyPair hskPath _hvkPath)) =
  [ "--node-id"
  , show nodeId
  , "--port"
  , show port
  , "--api-port"
  , show apiPort
  -- TODO(parenthetical): Not needed for now but put back at some point.
  -- , "--monitoring-port"
  -- , show monitoringPort
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
  , _verificationKey . _hydraKeys $ ni
  , "--cardano-verification-key"
  , _verificationKey . _cardanoKeys $ ni
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

devnetMagic :: Int
devnetMagic = 42
  {-
  output <- liftIO $ readCreateProcess devnetCp ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  where
    shellStr = intercalate " " ["./seed-devnet.sh", cardanoCliPath, hydraNodePath, jqPath, realpathPath]
    devnetCp =
      (shell shellStr)
      { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")]
      }
-}

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      flip runLoggingT (print . renderWithSeverity id) $ do
        prepareDevnet
        liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ _stdout _ _handle -> do
          flip runLoggingT (print . renderWithSeverity id) $ do
            logMessage $ WithSeverity Informational [i|
              Cardano node is running
              |]
            liftIO $ threadDelay $ seconds 2

            -- NOTE(skylar): Currently just using this as "did we do the setup?"
            envExists <- liftIO $ doesFileExist ".env"
            when (not envExists) $ do
              _ <- seedDevnet
              logMessage $ WithSeverity Informational [i|
              Devnet has been seeded
              |]
            hstxid <- publishReferenceScripts
            standupDemoHydraNetwork hstxid
            pure ()
          serve $ \case
            BackendRoute_Api :/ () -> do
              runWebSocketsSnap $ \pendingConnection -> do
                conn <- acceptRequest pendingConnection
                forkPingThread conn 30
                forever $ do
                  d <- receiveData conn
                  case d of
                    Just req -> do
                      r <- mkTaggedResponse req handleDemoApi
                      case r of
                        Left err -> error err
                        Right rsp ->
                          sendDataMessage conn $ Text (Aeson.encode rsp) Nothing
  
                    _ -> pure ()

                pure ()

            _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }

handleDemoApi :: DemoApi a -> IO a
handleDemoApi = \case
  DemoApi_GetWholeUTXO -> queryUTXOs

-- Suprised this doesn't exist :D
instance (ToJSON a, FromJSON a) => WebSocketsData (Maybe a) where
  fromDataMessage = Aeson.decode . fromDataMessage
  toLazyByteString = Aeson.encode

seconds :: Int -> Int
seconds = (* 1000000)
