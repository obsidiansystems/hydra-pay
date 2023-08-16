{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Config where

import System.FilePath
import HydraPay.Path
import HydraPay.Types
import HydraPay.Cardano.Hydra.ChainConfig (HydraChainConfig(..))
import Control.Lens hiding (argument)
import HydraPay.Logging
import HydraPay.Cardano.Node

data HydraPayConfig = HydraPayConfig
  { hydraPaySettings_database :: FilePath
  , hydraPaySettings_logSettings :: LogConfig
  , hydraPaySettings_nodeConfig :: NodeConfig
  }

data BootstrapState = BootstrapState
  { _bootstrap_nodeInfo :: NodeInfo
  , _bootstrap_logger :: Logger
  }

makeLenses ''BootstrapState

instance HasLogger BootstrapState where
  getLogger = bootstrap_logger

instance HasNodeInfo BootstrapState where
  nodeInfo = bootstrap_nodeInfo


mainnetScriptTxId :: TxId
mainnetScriptTxId = TxId "eb4c5f213ffb646046cf1d3543ae240ac922deccdc99826edd9af8ad52ddb877"

preprodScriptTxId :: TxId
preprodScriptTxId = TxId "85424831ee6a9dea9668990547df318a246ebda3e28a0ed60c5ca4b3de87e49f"

previewScriptTxId :: TxId
previewScriptTxId = TxId "058760d0370a965976a21ac9f11ace9b8f391d73f29b6d1b01bffdea7d0e1526"

sanchonetScriptTxId :: TxId
sanchonetScriptTxId = TxId "0000000000000000000000000000000000000000000000000000000000000000"

hydraChainConfig :: HydraChainConfig
hydraChainConfig =
  HydraChainConfig
  hydraChainGenesisShelley
  hydraChainProtocolParameters

preprodNodeConfig :: NodeConfig
preprodNodeConfig =
  NodeConfig
  preprodChainConfig
  dbPath
  (dbPath </> "node.socket")
  preprodChainTopology
  1
  preprodScriptTxId
  where
    dbPath = mkNodeDb "preprod"

preprodConfig :: HydraPayConfig
preprodConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  preprodNodeConfig

previewNodeConfig :: NodeConfig
previewNodeConfig =
  NodeConfig
  previewChainConfig
  dbPath
  (dbPath </> "node.socket")
  previewChainTopology
  2
  previewScriptTxId
  where
    dbPath = mkNodeDb "preview"

previewConfig :: HydraPayConfig
previewConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  previewNodeConfig

sanchonetNodeConfig :: NodeConfig
sanchonetNodeConfig =
  NodeConfig
  sanchonetChainConfig
  dbPath
  (dbPath </> "node.socket")
  sanchonetChainTopology
  4
  sanchonetScriptTxId
  where
    dbPath = mkNodeDb "sanchonet"

sanchonetConfig :: HydraPayConfig
sanchonetConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  sanchonetNodeConfig

mainnetNodeConfig :: NodeConfig
mainnetNodeConfig =
  NodeConfig
  mainnetChainConfig
  dbPath
  (dbPath </> "node.socket")
  mainnetChainTopology
  764824073
  mainnetScriptTxId
  where
    dbPath = mkNodeDb "mainnet"

mainnetConfig :: HydraPayConfig
mainnetConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  mainnetNodeConfig
