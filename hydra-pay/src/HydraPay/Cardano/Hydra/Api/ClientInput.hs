-- |

module HydraPay.Cardano.Hydra.Api.ClientInput where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Set (Set)


data ClientInput
  = Init
  | Abort
  | Commit {utxo :: Value}
  | NewTx {transaction :: Value}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic, Show, Eq)

instance FromJSON ClientInput
instance ToJSON ClientInput
