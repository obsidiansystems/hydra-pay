{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PaymentChannel where

import Data.Time
import Data.Int
import Data.Aeson
import Data.Text (Text)
import Data.Map (Map)

import Control.Concurrent.STM
import Control.Lens

import HydraPay.Cardano.Hydra.ChainConfig
import HydraPay.Cardano.Hydra.RunningHead
import qualified Cardano.Api as Api

import Database.Beam

data PaymentChannelConfig = PaymentChannelConfig
  { _paymentChannelConfig_name :: Text
  , _paymentChannelConfig_first :: Api.AddressAny
  , _paymentChannelConfig_second :: Api.AddressAny
  , _paymentChannelConfig_commitAmount :: Int32
  , _paymentChannelConfig_hydraChainConfig :: HydraChainConfig
  }

makeLenses ''PaymentChannelConfig

class HasPaymentChannelManager a where
  paymentChannelManager :: Lens' a PaymentChannelManager

-- | Manages running payment channels
data PaymentChannelManager = PaymentChannelManager
  { _paymentChannelManager_runningChannels :: TMVar (Map Int32 (TMVar RunningHydraHead))
  }

makeLenses ''PaymentChannelManager

data PaymentChannelStatus
  = PaymentChannelStatus_Unknown
  | PaymentChannelStatus_Created
  | PaymentChannelStatus_Initialized
  | PaymentChannelStatus_Open
  | PaymentChannelStatus_Closed
  | PaymentChannelStatus_Finalized
  | PaymentChannelStatus_Error
  | PaymentChannelStatus_Done
  deriving (Show, Eq, Ord, Read, Generic, Enum)

instance ToJSON PaymentChannelStatus
instance FromJSON PaymentChannelStatus

data PaymentChannelDisplayStatus
  = PaymentChannelDisplayStatus_Initializing
  | PaymentChannelDisplayStatus_WaitingForAccept
  | PaymentChannelDisplayStatus_WantsYourApproval
  | PaymentChannelDisplayStatus_Opening
  | PaymentChannelDisplayStatus_Open
  | PaymentChannelDisplayStatus_Closing
  | PaymentChannelDisplayStatus_Error
  deriving (Show, Eq, Ord, Read, Generic, Enum)

instance ToJSON PaymentChannelDisplayStatus
instance FromJSON PaymentChannelDisplayStatus

data PaymentChannelInfo = PaymentChannelInfo
  { _paymentChannelInfo_id :: Int32
  , _paymentChannelInfo_name :: Text
  , _paymentChannelInfo_createdAt :: UTCTime
  , _paymentChannelInfo_expiry :: UTCTime
  , _paymentChannelInfo_other :: Text
  , _paymentChannelInfo_status :: PaymentChannelDisplayStatus
  , _paymentChannelInfo_initiator :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelInfo
instance FromJSON PaymentChannelInfo

isPending :: PaymentChannelDisplayStatus -> Bool
isPending (PaymentChannelDisplayStatus_Initializing) = True
isPending _ = False

data TransactionDirection =
  TransactionReceived | TransactionSent
  deriving (Eq, Show, Enum, Generic)

instance ToJSON TransactionDirection
instance FromJSON TransactionDirection

data TransactionInfo = TransactionInfo
  { _transactionInfo_id :: Int32
  , _transactionInfo_time :: UTCTime
  , _transactionInfo_amount :: Int32
  , _transactionInfo_direction :: TransactionDirection
  }
  deriving (Eq, Show, Generic)

instance ToJSON TransactionInfo
instance FromJSON TransactionInfo

makeLenses ''TransactionInfo
makeLenses ''PaymentChannelInfo

paymentChannelDisplayName :: PaymentChannelInfo -> Text
paymentChannelDisplayName pinfo =
  case pinfo ^. paymentChannelInfo_status of
    PaymentChannelDisplayStatus_WantsYourApproval -> "New Request"
    _ -> pinfo ^. paymentChannelInfo_name
