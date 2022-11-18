{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Api where

import GHC.Generics
import Data.Aeson as Aeson
import qualified Data.Text as T

import Control.Lens.TH

import Hydra.Types
import Hydra.ServerOutput as ServerOutput

type HeadName = T.Text

data HydraPayStats = HydraPayStats
  { _hydraPayStats_heads :: Int
  , _hydraPayStats_nodes :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HydraPayStats
instance FromJSON HydraPayStats

data HeadCreate = HeadCreate
  { headCreate_name :: HeadName
  , headCreate_participants :: [Address]
  }
  deriving (Eq, Show, Generic)

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
  , headCommit_participant :: Address
  , headCommit_amount :: Lovelace
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


data Tagged a = Tagged
  { tagged_id :: Int
  , tagged_payload :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Tagged a)
instance FromJSON a => FromJSON (Tagged a)

data HydraPayError
  = InvalidPayload
  | HeadCreationFailed
  | NotEnoughParticipants
  | HeadExists HeadName
  | HeadDoesn'tExist
  | NetworkIsn'tRunning
  | FailedToBuildFundsTx
  | NodeCommandFailed
  -- ^ Anytime a command fails
  | NotAParticipant
  | NoValidUTXOToCommit
  | InsufficientFunds
  | FanoutNotPossible
  | TxInvalid {utxo :: WholeUTXO, transaction :: Value, validationError :: ValidationError}
  deriving (Eq, Show, Generic)

instance ToJSON HydraPayError
instance FromJSON HydraPayError

-- | State the head can be in, progressing linearly though the states.
data Status
  = Status_Pending
  | Status_Init
  | Status_Committing
  | Status_Open
  | Status_Closed
  | Status_Fanout
  | Status_Finalized
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Status
instance FromJSON Status

data ClientMsg
  = ClientHello
  | Authenticate T.Text
  | DoesHeadExist T.Text
  | CreateHead HeadCreate
  | InitHead HeadInit
  | CommitHead HeadCommit
  | CloseHead HeadName

  | TearDownHead HeadName
  -- ^ Kills network and removes head

  | CheckFuel Address
  | Withdraw Address
  | GetAddTx TxType Address Lovelace
  | SubscribeTo HeadName

  | RestartDevnet
  | GetStats

  | GetDevnetAddresses Int -- Amount of addresses
  deriving (Eq, Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

type Version = T.Text
data ServerMsg
  = ServerHello Version
  | FundsTx Tx
  | FuelAmount Lovelace
  | OperationSuccess
  | SubscriptionStarted HeadName
  | AlreadySubscribed HeadName
  | InvalidMessage
  | UnhandledMessage
  | HeadExistsResult Bool
  | DevnetRestarted
  | ServerError HydraPayError
  | HeadStatusChanged HeadName Status
  | NodeMessage (ServerOutput Value)
  | DevnetAddresses [Address]
  | CurrentStats HydraPayStats
  | RequestError T.Text
  | NotAuthenticated
  | AuthResult Bool
  deriving (Eq, Show, Generic)

instance ToJSON ServerMsg
instance FromJSON ServerMsg

data Tx = Tx
  { txType :: T.Text
  , txDescription :: T.Text
  , txCborHex :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tx where
  toJSON (Tx t d c) =
    object [ "type" .= t
           , "description" .= d
           , "cborHex" .= c
           ]

instance FromJSON Tx where
  parseJSON = withObject "Tx" $ \v -> Tx
    <$> v .: "type"
    <*> v .: "description"
    <*> v .: "cborHex"

data TxType =
  Funds | Fuel
  deriving (Eq, Show, Generic)

instance ToJSON TxType
instance FromJSON TxType

isFuelType :: TxType -> Bool
isFuelType Fuel = True
isFuelType _ = False

makeLenses ''HydraPayStats
