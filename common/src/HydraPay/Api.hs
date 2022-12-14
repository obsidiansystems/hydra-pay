{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Api where

import Data.Map
import Data.Int
import GHC.Generics
import Data.Aeson as Aeson
import qualified Data.Text as T
import Control.Applicative((<|>))

import Control.Lens.TH

import Data.Fixed (Pico)
import Hydra.Types
import Hydra.ServerOutput as ServerOutput
import qualified HydraPay.Config as Config

type HeadName = T.Text

data HydraPayStats = HydraPayStats
  { _hydraPayStats_heads :: Integer
  , _hydraPayStats_nodes :: Integer
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
  , headInit_contestation :: Integer
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
  , headStatus_balances :: Map Address Lovelace
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStatus
instance FromJSON HeadStatus


data Tagged a = Tagged
  { tagged_id :: Int64
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
  | ProcessError String
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
  | SubmitHeadTx Address HeadSubmitTx

  | RestartDevnet
  | GetStats

  | GetDevnetAddresses Integer -- Amount of addresses

  | GetL1Balance Address
  | GetHeadBalance HeadName Address

  | LiveDocEzSubmitTx Tx Address
  | GetIsManagedDevnet
  | GetHydraPayMode
  | GetProxyInfo Address
  deriving (Eq, Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

type Version = T.Text

versionStr :: Version
versionStr = "0.1.0"

data ServerMsg
  = ServerHello Version
  | OperationSuccess
  | HeadInfo HeadStatus
  | TxConfirmed Pico
  | FundsTx Tx
  | FuelAmount Lovelace
  | SubscriptionStarted HeadName
  | AlreadySubscribed HeadName
  | InvalidMessage T.Text
  | UnhandledMessage
  | HeadExistsResult Bool
  | DevnetRestarted
  | ServerError HydraPayError
  | HeadStatusChanged HeadName Status (Map Address Lovelace)
  | NodeMessage (ServerOutput Value)
  | DevnetAddresses [Address]
  | CurrentStats HydraPayStats
  | RequestError T.Text
  | NotAuthenticated
  | AuthResult Bool
  | L1Balance Lovelace
  | HeadBalance Lovelace
  | BalanceChange HeadName (Map Address Lovelace)
  | HeadRemoved HeadName
  | ApiError T.Text
  | HydraPayMode Config.HydraPayMode
  | IsManagedDevnet Bool
  | ProxyAddressInfo ProxyInfo
  deriving (Eq, Show, Generic)

instance ToJSON ServerMsg
instance FromJSON ServerMsg

-- | Information about the managed proxy-address
-- for a specific address
data ProxyInfo = ProxyInfo
  { proxyInfo_address :: Address
  , proxyInfo_proxyAddress :: Address
  , proxyInfo_balance :: Lovelace
  , proxyInfo_fuel :: Lovelace
  }
  deriving (Eq, Show, Generic)

instance ToJSON ProxyInfo
instance FromJSON ProxyInfo

data ApiMsg
  = TaggedMsg (Tagged ServerMsg)
  | PlainMsg ServerMsg

instance FromJSON ApiMsg where
  parseJSON v = (TaggedMsg <$> parseJSON v) <|> (PlainMsg <$> parseJSON v)

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
makePrisms ''ApiMsg
makePrisms ''ServerMsg
