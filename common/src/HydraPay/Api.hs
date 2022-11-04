module HydraPay.Api where

import GHC.Generics
import Data.Aeson as Aeson
import qualified Data.Text as T
import Hydra.Types

type HeadName = T.Text

-- TODO: All actions which are about a named head repeat this
-- fact. Use structure to aid code reuse, help to see patterns, and
-- encourage correct API use.

data HeadCreate = HeadCreate
  { headCreate_name :: HeadName
  , headCreate_participants :: [Address]
  , headCreate_startNetwork :: Bool
  }
  deriving (Generic)

instance ToJSON HeadCreate
instance FromJSON HeadCreate

data HeadInit = HeadInit
  { headInit_name :: HeadName
  , headInit_participant :: Address
  , headInit_contestation :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadInit
instance FromJSON HeadInit

data HeadCommit = HeadCommit
  { headCommit_name :: HeadName
  , headCommit_participant :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadCommit
instance FromJSON HeadCommit

data HeadSubmitTx = HeadSubmitTx
  { headSubmitTx_name :: HeadName
  , headSubmitTx_toAddr :: Address
  , amount :: Lovelace
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadSubmitTx
instance FromJSON HeadSubmitTx

-- This is the API json type that we need to send back out
data HeadStatus = HeadStatus
  { headStatus_name :: HeadName
  , headStatus_running :: Bool
  , headStatus_status :: Status
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStatus
instance FromJSON HeadStatus

data HydraPayError
  = InvalidPayload
  | HeadCreationFailed
  | NotEnoughParticipants
  | HeadExists HeadName
  | HeadDoesn'tExist
  | NetworkIsn'tRunning
  | FailedToBuildFundsTx
  | NotAParticipant
  | InsufficientFunds
  | FanoutNotPossible
  | TxInvalid {utxo :: WholeUTXO, transaction :: Value, validationError :: ValidationError}
  deriving (Eq, Show, Generic)

instance ToJSON HydraPayError
instance FromJSON HydraPayError

data Status
  = Status_Pending
  | Status_Init
  | Status_Committing
  | Status_Open
  | Status_Closed
  | Status_Fanout
  | Status_Finalized
  deriving (Eq, Show, Generic)

instance ToJSON Status
instance FromJSON Status
