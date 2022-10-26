-- | 

module Hydra.ServerOutput where

import GHC.Generics
import Data.Aeson

import Data.Time (UTCTime)
import Data.Text (Text)
import Hydra.Types
import Hydra.ClientInput

-- Copied and adapted from hydra-poc:hydra-node/src/hydra/ServerOutput.hs
-- Anything that took effort was replaced by Value

data ServerOutput tx
  = PeerConnected {peer :: Host}
  | PeerDisconnected {peer :: Host}
  | ReadyToCommit {parties :: Value}
  | Committed {party :: Party, utxo :: WholeUTXO}
  | HeadIsOpen {utxo :: WholeUTXO}
  | HeadIsClosed
      { snapshotNumber :: SnapshotNumber
      , -- | Nominal deadline until which contest can be submitted and after
        -- which fanout is possible. NOTE: Use this only for informational
        -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
        -- as the ledger of our cardano-node might not have progressed
        -- sufficiently in time yet and we do not re-submit transactions (yet).
        contestationDeadline :: UTCTime
      }
  | HeadIsContested {snapshotNumber :: SnapshotNumber}
  | ReadyToFanout
  | HeadIsAborted {utxo :: WholeUTXO}
  | HeadIsFinalized {utxo :: WholeUTXO}
  | CommandFailed {clientInput :: ClientInput}
  | TxSeen {transaction :: tx}
  | TxValid {transaction :: tx}
  | TxInvalid {utxo :: WholeUTXO, transaction :: tx, validationError :: ValidationError}
  | SnapshotConfirmed
      { snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  | GetUTxOResponse {utxo :: WholeUTXO}
  | InvalidInput {reason :: String, input :: Text}
  | -- | A friendly welcome message which tells a client something about the
    -- node. Currently used for knowing what signing key the server uses (it
    -- only knows one).
    Greetings {me :: Party}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RolledBack
  deriving (Eq, Show, Generic)

instance (ToJSON tx) => ToJSON (ServerOutput tx)
instance (FromJSON tx) => FromJSON (ServerOutput tx)
