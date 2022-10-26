-- | 

module Hydra.ClientInput where

import GHC.Generics
import Data.Aeson

import qualified Data.Text as T
import Hydra.Types

-- Copied and adapted from hydra-poc:hydra-node/src/hydra/ServerOutput.hs
-- Anything that took effort was replaced by Value

data ClientInput
  = Init {contestationPeriod :: ContestationPeriod}
  | Abort
  | Commit {utxo :: WholeUTXO}
  | NewTx {transaction :: T.Text}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Eq, Generic, Show)


instance ToJSON ClientInput
instance FromJSON ClientInput
