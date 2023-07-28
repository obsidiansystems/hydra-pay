-- | Paths used by app.

module HydraPay.Path where

bankAddrPath :: FilePath
bankAddrPath = "./bank.addr"

bankVkPath :: FilePath
bankVkPath = "./config/backend/bank.cardano.vk"

bankSkPath :: FilePath
bankSkPath = "./config/backend/bank.cardano.sk"

hydraChainGenesisShelley :: FilePath
hydraChainGenesisShelley = "config/backend/hydra/genesis-shelley.json"

hydraChainProtocolParameters :: FilePath
hydraChainProtocolParameters = "config/backend/hydra/protocol-parameters.json"

hydraPayDb :: FilePath
hydraPayDb = "hydra-pay-db"

cardanoNodeDb :: FilePath
cardanoNodeDb = "cardano-node-db"

previewChainConfig :: FilePath
previewChainConfig = "config/backend/preview/config.json"

previewChainTopology :: FilePath
previewChainTopology = "config/backend/preview/topology.json"
