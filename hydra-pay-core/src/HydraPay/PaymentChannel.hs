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
  = PaymentChannelOpen
  | PaymentChannelPending UTCTime
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelStatus
instance FromJSON PaymentChannelStatus

data PaymentChannelInfo = PaymentChannelInfo
  { _paymentChannelInfo_id :: Int32
  , _paymentChannelInfo_name :: Text
  , _paymentChannelInfo_other :: Text
  , _paymentChannelInfo_status :: PaymentChannelStatus
  , _paymentChannelInfo_initiator :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelInfo
instance FromJSON PaymentChannelInfo

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
  case paymentChannelNeedsMyAcceptance pinfo of
    True -> "New Request"
    False -> pinfo ^. paymentChannelInfo_name

paymentChannelNeedsMyAcceptance :: PaymentChannelInfo -> Bool
paymentChannelNeedsMyAcceptance pinfo =
  pinfo ^. paymentChannelInfo_status . to isPending && not (pinfo ^. paymentChannelInfo_initiator)

isPending :: PaymentChannelStatus -> Bool
isPending (PaymentChannelPending _) = True
isPending _ = False
