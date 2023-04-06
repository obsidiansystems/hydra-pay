{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Node where

import Data.Int
import System.IO
import System.Which
import System.Process
import Control.Lens

data NodeConfig = NodeConfig
  { _nodeConfig_config :: FilePath
  , _nodeConfig_databasePath :: FilePath
  , _nodeConfig_socketPath :: FilePath
  , _nodeConfig_topology :: FilePath
  , _nodeConfig_magic :: Int32
  }
  deriving (Eq, Show)

makeLenses ''NodeConfig

data NodeInfo = NodeInfo
  { _nodeInfo_socket :: FilePath
  , _nodeInfo_magic :: Int32
  }
  deriving (Eq, Show)

makeLenses ''NodeInfo

withCardanoNode :: NodeConfig -> (NodeInfo -> IO a) -> IO a
withCardanoNode cfg action = do
  withFile (cfg ^. nodeConfig_databasePath <> "/node.log") AppendMode $ \outHandle -> do
    withFile (cfg ^. nodeConfig_databasePath <> "/node_error.log") AppendMode $ \errHandle -> do
      withCreateProcess (makeNodeProcess outHandle errHandle cfg) $ \_ out err ph -> do
        action $ NodeInfo (cfg ^. nodeConfig_socketPath) (cfg ^. nodeConfig_magic)

makeNodeProcess :: Handle -> Handle -> NodeConfig -> CreateProcess
makeNodeProcess outHandle errHandle (NodeConfig configPath dbPath socketPath topo _) =
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
