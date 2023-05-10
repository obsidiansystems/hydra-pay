-- |

module HydraPay.Cardano.Hydra.Api
  ( module HydraPay.Cardano.Hydra.Api
  , module X
  )
  where

import GHC.Generics
import Data.Aeson
import Data.Int (Int32)
import Data.Text (Text)
import Data.Set (Set)
import HydraPay.Cardano.Hydra.Api.ClientInput as X hiding (utxo, transaction)

type HeadId = Int32

data ServerOutput
  = PeerConnected {peer :: Value}
  | PeerDisconnected {peer :: Value}
  | HeadIsInitializing {headId :: HeadId, parties :: Set Value}
  | Committed {headId :: HeadId, party :: Value, utxo :: Value}
  | HeadIsOpen {headId :: HeadId, utxo :: Value}
  | HeadIsClosed
      { headId :: HeadId
      , snapshotNumber :: Value
      , -- | Nominal deadline until which contest can be submitted and after
        -- which fanout is possible. NOTE: Use this only for informational
        -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
        -- as the ledger of our cardano-node might not have progressed
        -- sufficiently in time yet and we do not re-submit transactions (yet).
        contestationDeadline :: Value
      }
  | HeadIsContested {headId :: HeadId, snapshotNumber :: Value}
  | ReadyToFanout {headId :: HeadId}
  | HeadIsAborted {headId :: HeadId, utxo :: Value}
  | HeadIsFinalized {headId :: HeadId, utxo :: Value}
  | CommandFailed {clientInput :: ClientInput}
  | -- | Given transaction has been seen as valid in the Head. It is expected to
    -- eventually be part of a 'SnapshotConfirmed'.
    TxValid {headId :: HeadId, transaction :: Value}
  | -- | Given transaction was not not applicable to the given UTxO in time and
    -- has been dropped.
    TxInvalid {headId :: HeadId, utxo :: Value, transaction :: Value, validationError :: Value}
  | -- | Given snapshot was confirmed and included transactions can be
    -- considered final.
    SnapshotConfirmed
      { headId :: HeadId
      , snapshot :: Value
      , signatures :: Value
      }
  | GetUTxOResponse {headId :: HeadId, utxo :: Value}
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
