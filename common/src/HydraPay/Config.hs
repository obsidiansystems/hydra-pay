module HydraPay.Config where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)

data HydraPayConfig = HydraPayConfig
  { _hydraPayMode :: HydraPayMode
  , _port :: Int
  , _bind :: String
  }
  deriving (Show,Read)

-- | Configure Cardano (L1) and Hydra networks. This is either an
-- explicit configuration for Cardano Node and Hydra Nodes, or the
-- default mode which runs a devnet for live documentation.
data HydraPayMode
  = ManagedDevnetMode
  | ConfiguredMode
    { _cardanoNodeParams :: CardanoNodeParams
    , _hydraNodeParams :: HydraNodeParams
    }
  deriving (Eq,Show,Read,Generic)

instance ToJSON HydraPayMode
instance FromJSON HydraPayMode

data CardanoNodeParams = CardanoNodeParams
  { _testnetMagic :: Int
  , _nodeSocket :: FilePath
  , _ledgerGenesis :: FilePath
  }
  deriving (Eq,Show,Read,Generic)

instance ToJSON CardanoNodeParams
instance FromJSON CardanoNodeParams

data HydraNodeParams = HydraNodeParams
  { _hydraScriptsTxId :: String
  , _hydraLedgerProtocolParameters :: FilePath
  , _hydraLedgerGenesis :: FilePath
  }
  deriving (Eq,Show,Read,Generic)

instance ToJSON HydraNodeParams
instance FromJSON HydraNodeParams
