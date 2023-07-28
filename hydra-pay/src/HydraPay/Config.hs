module HydraPay.Config where

import qualified Cardano.Api as Api
import System.FilePath

-- import HydraPay.HydraPayConfig (HydraPayConfig(..))
import HydraPay.Cardano.Hydra.ChainConfig (HydraChainConfig(..))
import HydraPay.Cardano.Node (NodeConfig(..))
import HydraPay.Logging (defaultLogConfig)

nominalFuel :: Api.Lovelace
nominalFuel = 100000000

topUpLevel :: Api.Lovelace
topUpLevel = 60000000
