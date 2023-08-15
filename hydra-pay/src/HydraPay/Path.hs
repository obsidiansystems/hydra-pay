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
previewChainConfig = "config/preview/config.json"

previewChainTopology :: FilePath
previewChainTopology = "config/preview/topology.json"

sanchonetChainConfig :: FilePath
sanchonetChainConfig = "config/sanchonet/config.json"

sanchonetChainTopology :: FilePath
sanchonetChainTopology = "config/sanchonet/topology.json"

preprodChainConfig :: FilePath
preprodChainConfig = "config/preprod/config.json"

preprodChainTopology :: FilePath
preprodChainTopology = "config/preprod/topology.json"

mainnetChainConfig :: FilePath
mainnetChainConfig = "config/mainnet/config.json"

mainnetChainTopology :: FilePath
mainnetChainTopology = "config/mainnet/topology.json"
