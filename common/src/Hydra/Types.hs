-- | 

module Hydra.Types where

import GHC.Generics
import Data.Aeson

import Data.Map (Map)
import qualified Data.Text as T

-- | Cardano address
type Address = String

type TxIn = T.Text
type WholeUTXO = Map TxIn TxInInfo

data TxInInfo = TxInInfo
  { address :: Address
  , datumhash :: Maybe T.Text
  , value :: Map T.Text Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON TxInInfo
instance FromJSON TxInInfo

fuelMarkerDatumHash :: T.Text
fuelMarkerDatumHash = "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
