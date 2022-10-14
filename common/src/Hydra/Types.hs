-- | 

module Hydra.Types where

import GHC.Generics
import Data.Aeson

import Data.Map (Map)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Text (Text)
import Numeric.Natural (Natural)

-- | Cardano address
type Address = String

type Lovelace = Int

type TxIn = T.Text
type WholeUTXO = Map TxIn TxInInfo

data TxInInfo = TxInInfo
  { address :: Address
  , datumhash :: Maybe T.Text
  , value :: Map T.Text Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON TxInInfo
instance ToJSON TxInInfo

fuelMarkerDatumHash :: T.Text
fuelMarkerDatumHash = "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"


-- REVIEW(SN): This is also used in hydra-tui

-- Below various types copied/adapted from hydra-poc code

data Host = Host
  { hostname :: Text
  , port :: PortNumber
  }
  deriving (Ord, Generic, Eq, Show)

instance ToJSON Host
instance FromJSON Host


newtype Party = Party {vkey :: T.Text}
  deriving (Eq, Show, Read, Generic)

instance ToJSON Party
instance FromJSON Party

type UTxOType tx = Value
type Snapshot tx = Value
type MultiSignature x = Value
type PostChainTx tx = Value 
type PostTxError tx = Value
type ValidationError = Value
type SnapshotNumber = Natural
type PortNumber = Natural
type ContestationPeriod = Natural
