{-# Language TemplateHaskell #-}
module HydraPay.Cardano.Hydra.ChainConfig where

import Control.Lens

data HydraChainConfig = HydraChainConfig
  { _hydraChainConfig_ledgerGenesis :: FilePath
  , _hydraChainConfig_ledgerProtocolParams :: FilePath
  }

makeLenses ''HydraChainConfig
