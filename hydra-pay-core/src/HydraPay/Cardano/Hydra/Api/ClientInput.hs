-- |

module HydraPay.Cardano.Hydra.Api.ClientInput where

import GHC.Generics
import Data.Aeson


data ClientInput
  = Init
  | Abort
  | Commit {utxo :: Value}
  | NewTx {transaction :: Value }
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic, Show, Eq)

instance FromJSON ClientInput
instance ToJSON ClientInput

data DraftCommitTxRequest = DraftCommitTxRequest
  { utxos :: Value
  }
  deriving (Generic)

instance FromJSON DraftCommitTxRequest
instance ToJSON DraftCommitTxRequest
