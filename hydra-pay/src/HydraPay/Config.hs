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


-- | Transaction ID on preview from the Hydra 0.11.0 release
-- https://github.com/input-output-hk/hydra/releases/tag/0.11.0
mainnetScriptTxId :: TxId
mainnetScriptTxId = TxId "eb4c5f213ffb646046cf1d3543ae240ac922deccdc99826edd9af8ad52ddb877"

preprodScriptTxId :: TxId
preprodScriptTxId = TxId "010f68ad75cda7983b68a7691ba1591fa9ce4cfc03ac35d1c6c90cae4b48f849"

previewScriptTxId :: TxId
previewScriptTxId = TxId "90acbeb0ebece3b5319625eedca3f6514870c9414872d9e940c6b7d7b88178fd"

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
  cardanoNodeDb
  (cardanoNodeDb </> "node.socket")
  preprodChainTopology
  1
  preprodScriptTxId

preprodConfig :: HydraPayConfig
preprodConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  preprodNodeConfig

previewNodeConfig :: NodeConfig
previewNodeConfig =
  NodeConfig
  previewChainConfig
  cardanoNodeDb
  (cardanoNodeDb </> "node.socket")
  previewChainTopology
  2
  previewScriptTxId

previewConfig :: HydraPayConfig
previewConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  previewNodeConfig

sanchonetNodeConfig :: NodeConfig
sanchonetNodeConfig =
  NodeConfig
  sanchonetChainConfig
  cardanoNodeDb
  (cardanoNodeDb </> "node.socket")
  sanchonetChainTopology
  4
  sanchonetScriptTxId

sanchonetConfig :: HydraPayConfig
sanchonetConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  sanchonetNodeConfig

mainnetNodeConfig :: NodeConfig
mainnetNodeConfig =
  NodeConfig
  mainnetChainConfig
  cardanoNodeDb
  (cardanoNodeDb </> "node.socket")
  mainnetChainTopology
  764824073
  mainnetScriptTxId

mainnetConfig :: HydraPayConfig
mainnetConfig = HydraPayConfig
  hydraPayDb
  (defaultLogConfig "hydra-pay")
  mainnetNodeConfig
