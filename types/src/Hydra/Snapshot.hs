module Hydra.Snapshot where
import GHC.Natural (Natural)
import Hydra.Types (WholeUTXO)
import GHC.Generics (Generic)
import Data.Aeson.Types

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { snapshotNumber :: SnapshotNumber
  , utxo :: WholeUTXO
  , -- | The set of transactions that lead to 'utxo'
    confirmedTransactions :: [tx]
  }
  deriving (Eq, Show, Generic)

instance (ToJSON tx) => ToJSON (Snapshot tx)
instance (FromJSON tx) => FromJSON (Snapshot tx)
