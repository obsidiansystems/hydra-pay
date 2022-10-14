{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Backend

(backend)

where

import Prelude hiding (filter)

import Hydra.Devnet

import Common.Route
import Common.Api
import Control.Monad
import Obelisk.Backend
import Obelisk.Route

import System.Directory
import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Witherable
import Data.String.Interpolate ( i, iii )
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import Control.Concurrent
import System.Process

import Data.Aeson as Aeson
    ( decode, encode, ToJSON, FromJSON, (.:), withObject, Value )


import Network.WebSockets
import Network.WebSockets.Snap

import Reflex.Dom.GadtApi.WebSocket

import qualified Data.Map.Merge.Lazy as Map
import qualified Hydra.Types as HT
import Data.Maybe (fromJust, fromMaybe)
import Data.Aeson.Types (parseMaybe)
import System.IO (IOMode(WriteMode), openFile)
import Data.IORef (readIORef, writeIORef, IORef, newIORef)
import Hydra.Types (Address)
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8

standupDemoHydraNetwork :: (MonadIO m)
  => HydraScriptTxId
  -> Map Text HydraKeyInfo
  -> m (Map Text (ProcessHandle, HydraNodeInfo))
standupDemoHydraNetwork hstxid actors = do
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

-- | Takes the node participant and the list of peers
mkHydraNodeCP :: HydraSharedInfo -> HydraNodeInfo -> [HydraNodeInfo] -> CreateProcess
mkHydraNodeCP sharedInfo node peers =
  (proc hydraNodePath $ sharedArgs sharedInfo <> nodeArgs node <> concatMap peerArgs peers)
  { std_out = Inherit
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

runHydraDemo :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m)
  => HydraDemo
  -> m (Map Text ( ProcessHandle
                   , Address -- Cardano address
                   , HydraNodeInfo
                   ))
runHydraDemo nodes = do
  keysAddresses <- forM nodes $ \(actorSeed, fuelSeed) -> do
    keys@(HydraKeyInfo (KeyPair _ vk) _) <- generateKeys
    addr <- liftIO $ getCardanoAddress vk
    void $ seedAddressFromFaucetAndWait addr actorSeed False
    void $ seedAddressFromFaucetAndWait addr fuelSeed True
    pure (keys, addr)
  logMessage $ WithSeverity Informational "Publishing reference scripts"
  hstxid <- publishReferenceScripts
  handles <- standupDemoHydraNetwork hstxid (fmap fst keysAddresses)
  logMessage $ WithSeverity Informational [i|"Hydra Network Running for nodes #{Map.keys nodes}|]
  pure $ Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (\_ addr (handle, nodeInfo) -> (handle, addr, nodeInfo))) (fmap snd keysAddresses) handles


type BackendState = Map Text ( ProcessHandle
                             , Address -- Cardano address
                             , HydraNodeInfo
                             )

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      hydraProcessHandlesRef :: IORef BackendState <- liftIO (newIORef mempty)
      flip runLoggingT (print . renderWithSeverity id) $ do
        prepareDevnet
        liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ _stdout _ _handle -> do
          flip runLoggingT (print . renderWithSeverity id) $ do
            logMessage $ WithSeverity Informational [i|
              Cardano node is running
              |]
            liftIO $ threadDelay $ seconds 3
            liftIO . serve $ \case
              BackendRoute_Api :/ () -> do
                 runWebSocketsSnap $ \pendingConnection -> do
                  conn <- acceptRequest pendingConnection
                  forkPingThread conn 30
                  forever $ do
                    d <- receiveData conn
                    case d of
                      Just req -> do
                        r <- mkTaggedResponse req (handleDemoApi hydraProcessHandlesRef)
                        case r of
                          Left err -> do
                            putStrLn [iii|Error mkTaggedResponse: #{err}|]
                            error err
                          Right rsp ->
                            sendDataMessage conn $ Text (Aeson.encode rsp) Nothing

                      _ -> pure ()

                  pure ()
              _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }

handleDemoApi :: IORef BackendState
              -> DemoApi a
              -> IO a
handleDemoApi hydraProcessHandlesRef = \case
  DemoApi_GetActorUTXO addr -> queryAddressUTXOs addr
  DemoApi_MkTx fromName utxos lovelace toName -> do
    print (fromName, utxos, toName)
    let lovelaceUtxos = mapMaybe (Map.lookup "lovelace" . HT.value) utxos
    actors <- readIORef hydraProcessHandlesRef
    jsonStr <- buildSignedHydraTx
               (_signingKey . _cardanoKeys . _keys . (\(_,_,hn) -> hn) $ actors ! fromName)
               ((\(_,addr,_) -> addr) $ actors ! fromName)
               ((\(_,addr,_) -> addr) $ actors ! toName)
               lovelaceUtxos
               lovelace
    let jsonTx :: Value = fromMaybe (error "Failed to parse TX") . Aeson.decode . ByteString.Char8.pack $ jsonStr
    pure . fromJust . parseMaybe (withObject "signed tx" (.: "cborHex")) $ jsonTx
  DemoApi_Start demo -> do
    liftIO (mapM (terminateProcess . (\(hndl,_,_) -> hndl)) =<< readIORef hydraProcessHandlesRef)
    nodeInfos <- runLoggingT (runHydraDemo demo) (print . renderWithSeverity id)
    liftIO . writeIORef hydraProcessHandlesRef $ nodeInfos
    actorList :: RunningNodes <- forM nodeInfos $ \(_, addr, nInfo) -> do
            pure ( addr
                 , [iii|ws://localhost:#{_apiPort nInfo}|]
                 )
    pure actorList


-- Suprised this doesn't exist :D
instance (ToJSON a, FromJSON a) => WebSocketsData (Maybe a) where
  fromDataMessage = Aeson.decode . fromDataMessage
  toLazyByteString = Aeson.encode

seconds :: Int -> Int
seconds = (* 1000000)
