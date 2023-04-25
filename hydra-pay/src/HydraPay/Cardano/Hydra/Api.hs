-- |

module HydraPay.Cardano.Hydra.Api where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Set (Set)

data ClientInput
  = Init
  | Abort
  | Commit Value
  | NewTx Value
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic, Show, Eq)

instance FromJSON ClientInput
instance ToJSON ClientInput

data ServerOutput
  = PeerConnected {peer :: Value}
  | PeerDisconnected {peer :: Value}
  | HeadIsInitializing {headId :: Value, parties :: Set Value}
  | Committed {headId :: Value, party :: Value, utxo :: Value}
  | HeadIsOpen {headId :: Value, utxo :: Value}
  | HeadIsClosed
      { headId :: Value
      , snapshotNumber :: Value
      , -- | Nominal deadline until which contest can be submitted and after
        -- which fanout is possible. NOTE: Use this only for informational
        -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
        -- as the ledger of our cardano-node might not have progressed
        -- sufficiently in time yet and we do not re-submit transactions (yet).
        contestationDeadline :: Value
      }
  | HeadIsContested {headId :: Value, snapshotNumber :: Value}
  | ReadyToFanout {headId :: Value}
  | HeadIsAborted {headId :: Value, utxo :: Value}
  | HeadIsFinalized {headId :: Value, utxo :: Value}
  | CommandFailed {clientInput :: ClientInput}
  | -- | Given transaction has been seen as valid in the Head. It is expected to
    -- eventually be part of a 'SnapshotConfirmed'.
    TxValid {headId :: Value, transaction :: Value}
  | -- | Given transaction was not not applicable to the given UTxO in time and
    -- has been dropped.
    TxInvalid {headId :: Value, utxo :: Value, transaction :: Value, validationError :: Value}
  | -- | Given snapshot was confirmed and included transactions can be
    -- considered final.
    SnapshotConfirmed
      { headId :: Value
      , snapshot :: Value
      , signatures :: Value
      }
  | GetUTxOResponse {headId :: Value, utxo :: Value}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one).
    Greetings {me :: Value}
  | PostTxOnChainFailed {postChainTx :: Value, postTxError :: Value}
  | RolledBack
  deriving (Generic, Show)

instance ToJSON ServerOutput
instance FromJSON ServerOutput
