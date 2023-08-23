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
  , _paymentChannelConfig_useProxies :: Bool
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
  = PaymentChannelStatus_Submitting
  | PaymentChannelStatus_WaitingForAccept
  | PaymentChannelStatus_Opening
  | PaymentChannelStatus_Open
  | PaymentChannelStatus_Closing
  | PaymentChannelStatus_Done
  | PaymentChannelStatus_Error
  deriving (Show, Eq, Ord, Read, Generic, Enum)

instance ToJSON PaymentChannelStatus
instance FromJSON PaymentChannelStatus

data PaymentChannelInfo = PaymentChannelInfo
  { _paymentChannelInfo_id :: Int32
  , _paymentChannelInfo_name :: Text
  , _paymentChannelInfo_createdAt :: UTCTime
  , _paymentChannelInfo_expiry :: UTCTime
  , _paymentChannelInfo_other :: Text
  , _paymentChannelInfo_status :: PaymentChannelStatus
  , _paymentChannelInfo_initiator :: Bool
  , _paymentChannelInfo_balance :: Maybe Api.Lovelace
  }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelInfo
instance FromJSON PaymentChannelInfo

isPending :: PaymentChannelStatus -> Bool
isPending (PaymentChannelStatus_Submitting) = True
isPending (PaymentChannelStatus_WaitingForAccept) = True
isPending (PaymentChannelStatus_Opening) = True
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
    PaymentChannelStatus_WaitingForAccept | pinfo ^. paymentChannelInfo_initiator . to not -> "New Request"
    _ -> pinfo ^. paymentChannelInfo_name
