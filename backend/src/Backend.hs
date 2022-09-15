{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend where

import Common.Route
import Control.Monad
import Obelisk.Backend

import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import GHC.IO.Handle

import Control.Exception
import Control.Concurrent
import System.Process
import System.Which

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

jqPath :: FilePath
jqPath = $(staticWhich "jq")

prepareDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ()
prepareDevnet = do
  output <- liftIO $ readCreateProcess (shell "[ -d devnet ] || ./demo/prepare-devnet.sh") ""
  when (length output > 0) $ logMessage $ WithSeverity Informational $ pretty $ T.pack output

participants :: [String]
participants = ["alice", "bob", "carol"]

standupHydraNetwork :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => [String] -> m [ProcessHandle]
standupHydraNetwork names = do
  -- TODO(skylar): Read the env file first
  env <- readEnv ".env"
  result <- liftIO $ mapM (createProcessGiveHandle) $ generateHydraNetworkCPs env names
  logMessage $ WithSeverity Informational $ "Hydra Network Running for peers: " <> pretty names
  pure result
  where
    createProcessGiveHandle cp = do
      (_,_,_,handle) <- createProcess cp
      pure handle

-- | Given the names of the participants generate the CreateProcess for all hydra-nodes
generateHydraNetworkCPs :: [(String, String)] -> [String] -> [CreateProcess]
generateHydraNetworkCPs env names = map (\p -> mkHydraNodeCP env p (filter (/=p) peers)) peers
  where
    peers = zip [1..] names

-- TODO(skylar): What about failure??
-- We can either fail by having the file be not found, or when we can't parse
-- for the parse part we should use attoparsec or something...
readEnv :: MonadIO m => FilePath -> m ([(String, String)])
readEnv envFilePath = liftIO $ parseEnvVars <$> readFile envFilePath
  where
    parseEnvVars = fmap (\x -> (parseVarName x, parseVarVal x)) . lines

    parseVarName = takeWhile (/= '=')
    parseVarVal = drop 1 . dropWhile (/= '=')

-- | Takes the node participant and the list of peers
mkHydraNodeCP :: [(String, String)] -> (Int, String) -> [(Int, String)] -> CreateProcess
mkHydraNodeCP env (pid, name) peers =
  (proc hydraNodePath
  (["--node-id", show pid,
    "--port", "500" <> show pid,
    "--api-port", "400" <> show pid,
    "--monitoring-port", "600" <> show pid,
    "--hydra-signing-key", name <> ".sk",
    "--hydra-scripts-tx-id",  "5f15831e663dd545cdb746a29c174e31544413b1cdb755245549dcf7bfc26485",
    "--cardano-signing-key", "devnet/credentials/" <> name <> ".sk",
    "--ledger-genesis", "devnet/genesis-shelley.json",
    "--ledger-protocol-parameters", "devnet/protocol-parameters.json",
    "--network-id", "42",
    "--node-socket", "/home/yasuke/code/ob-hydra-poc/devnet/node.socket"
  ] <> mconcat (map peerArgs peers))) { std_out = CreatePipe
                                      }
  where
    peerArgs (peerId, peerName) =
      [ "--peer", "127.0.0.1:500" <> show peerId
      , "--hydra-verification-key", peerName <> ".vk"
      , "--cardano-verification-key", "devnet/credentials/" <> peerName <> ".vk"
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

spawnCardanoNode :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ProcessHandle
spawnCardanoNode = do
  (_, stdout, _, pc) <- liftIO $ createProcess cardanoNodeCreateProcess
  pure pc

seedDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ()
seedDevnet = do
  output <- liftIO $ readCreateProcess devnetCp ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  where
    devnetCp =
      (shell ("./seed-devnet.sh " <> cardanoCliPath <> " " <> hydraNodePath <> " " <> jqPath))
      { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")]
      }

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      flip runLoggingT (print . renderWithSeverity id) $ do
        prepareDevnet
        liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ stdout _ handle -> do
          flip runLoggingT (print . renderWithSeverity id) $ do
            logMessage $ WithSeverity Informational "Cardano node is running\nIf you need to seed run the ./seed-devnet.sh command"
            -- NOTE(skylar): Delay or the socket fails
            liftIO $ threadDelay $ seconds 2
            standupHydraNetwork  participants

          serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

seconds :: Int -> Int
seconds = (* 1000000)
