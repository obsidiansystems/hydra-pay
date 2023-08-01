{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Node where

import Data.Int
import qualified Data.Text as T

import HydraPay.Types
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import System.IO
import System.Which
import System.Process

import Control.Lens hiding (parts)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Reflex
import Reflex.Host.Headless

import qualified HydraPay.Watch as Watch

import Control.Concurrent
import qualified System.FSNotify as FS

data NodeConfig = NodeConfig
  { _nodeConfig_config :: FilePath
  , _nodeConfig_databasePath :: FilePath
  , _nodeConfig_socketPath :: FilePath
  , _nodeConfig_topology :: FilePath
  , _nodeConfig_magic :: Int32
  , _nodeConfig_hydraScriptsTxId :: TxId
  }
  deriving (Eq, Show)

makeLenses ''NodeConfig

data NodeInfo = NodeInfo
  { _nodeInfo_socketPath :: FilePath
  , _nodeInfo_magic :: Int32
  , _nodeInfo_socketReady :: TMVar ()
  , _nodeInfo_hydraScriptsTxId :: TxId
  }
  deriving (Eq)

makeLenses ''NodeInfo

class HasNodeInfo a where
  nodeInfo :: Lens' a NodeInfo

instance HasNodeInfo NodeInfo where
  nodeInfo = id

pathAndFile :: FilePath -> (String, String)
pathAndFile fp =
  case parts of
    (a:[]) -> ("./", T.unpack a)
    (a:b:[]) -> (T.unpack a,T.unpack b)
    _ -> (T.unpack . T.intercalate "/" . init $ parts, T.unpack $ last parts)
  where
    parts = T.splitOn "/" . T.pack $ fp

-- | Predicate to alert us about the socket created by cardano-node
socketPred :: FilePath -> FS.Event -> Bool
socketPred file (FS.Added path _ _) | T.isSuffixOf (T.pack file) (T.pack path) = True
socketPred _ _ = False

ensureNodeSocket :: (HasNodeInfo a, MonadIO m) => a -> m ()
ensureNodeSocket a = liftIO $ do
  atomically $ readTMVar (a ^. nodeInfo . nodeInfo_socketReady)

withCardanoNode :: NodeConfig -> (NodeInfo -> IO a) -> IO a
withCardanoNode cfg action = do
  createDirectoryIfMissing True $ cfg ^. nodeConfig_databasePath
  protocolMagicIdExists <- doesFileExist $ cfg ^. nodeConfig_databasePath </> "protocolMagicId"
  when (not protocolMagicIdExists) $
    writeFile (cfg ^. nodeConfig_databasePath </> "protocolMagicId") $ show $ cfg ^. nodeConfig_magic
  withFile (cfg ^. nodeConfig_databasePath <> "/node.log") AppendMode $ \outHandle -> do
    withFile (cfg ^. nodeConfig_databasePath <> "/node_error.log") AppendMode $ \errHandle -> do
      withCreateProcess (makeNodeProcess outHandle errHandle cfg) $ \_ _ _ ph -> do
        ready <- newEmptyTMVarIO
        let
          ninfo = NodeInfo (cfg ^. nodeConfig_socketPath) (cfg ^. nodeConfig_magic) ready (cfg ^. nodeConfig_hydraScriptsTxId)
          (path, file) = pathAndFile $ ninfo ^. nodeInfo_socketPath

        -- Fork the FRP layer so we can still run the action
        socketWatchThreadId <- forkIO $ runHeadlessApp $ do
          pb <- getPostBuild
          exited <- performEventAsync $ ffor pb $ \_ cb -> do
            liftIO $ void $ forkIO $ cb =<< waitForProcess ph
          (changed, watchThread) <- Watch.watchDir FS.defaultConfig path $ socketPred file
          performEvent_ $ ffor changed $ const $ do
            liftIO $ atomically $ putTMVar ready ()
          performEvent_ $ ffor exited $ \_ -> liftIO $ killThread watchThread
          delay 0.1 $ () <$ exited

        action ninfo `finally` (killThread socketWatchThreadId)

makeNodeProcess :: Handle -> Handle -> NodeConfig -> CreateProcess
makeNodeProcess outHandle errHandle (NodeConfig configPath dbPath socketPath topo _ _) =
  cardanoCp { std_out = UseHandle outHandle
            , std_err = UseHandle errHandle
            }
  where
    cardanoCp = proc cardanoNodePath
      ["run"
      , "--topology"
      , topo
      , "--database-path"
      , dbPath
      , "--socket-path"
      , socketPath
      , "--config"
      , configPath
      , "--host-addr"
      , "0.0.0.0"
      , "--port"
      , "3001"
      ]

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")
