-- |

module HydraPay.Cardano.Hydra.Api.ClientInput where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

data DraftCommitTxRequest
  = DraftCommitTxRequest { utxos :: Value }
  | DraftCommitRequestPlaceholder
  -- ^ Ensure we encode a "tag"
  deriving (Generic)

instance FromJSON DraftCommitTxRequest
instance ToJSON DraftCommitTxRequest

data DraftCommitTxResponse
  = DraftCommitTxResponse { commitTx :: Text }
  | DraftCommitResponsePlaceholder
  -- ^ Ensure we encode a "tag"
  deriving (Generic)

instance FromJSON DraftCommitTxResponse
instance ToJSON DraftCommitTxResponse
