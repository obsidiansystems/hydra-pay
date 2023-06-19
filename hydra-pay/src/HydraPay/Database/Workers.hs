-- | Tracking Hydra Tasks from a database.

{-# LANGUAGE TemplateHaskell #-}
module HydraPay.Database.Workers where

import ByteString.Aeson.Orphans ()
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Proxy
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.ByteString.Char8 as BS
import Database.Beam
import qualified Database.Beam.AutoMigrate as Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

data RefundRequest = RefundRequest
  { _refundRequest_hydraHead :: Int32
  , _refundRequest_hydraAddress :: Text
  , _refundRequest_signingKeyPath :: FilePath
  , _refundRequest_chainAddress :: Text
  , _refundRequest_amount :: Int32
  , _refundRequest_protocolParams :: Text
  }
  deriving Generic

instance FromJSON RefundRequest
instance ToJSON RefundRequest

data PaymentChannelReq
  = PaymentChannelReq_Init Int32 Text
  -- ^ The head id used to find the data required on the db to fullfill the Open
  -- Channel request.
  | PaymentChannelReq_Accept Int32
  | PaymentChannelReq_Close Int32 Text
  | PaymentChannelReq_InitiatorRefund RefundRequest
  | PaymentChannelReq_Fund Int32 [FundRequest] -- Proxy ids to fund
  | PaymentChannelReq_Join Int32 Text Api.Lovelace
  | PaymentChannelReq_RetryInit Int32
  | PaymentChannelReq_RetryClose Int32 Text
  | PaymentChannelReq_SpinUpHead Int32
  | PaymentChannelReq_Commit Int32 Text Api.Lovelace
  | PaymentChannelReq_Fanout Int32
  | PaymentChannelReq_Cleanup Int32
  deriving Generic

data FundRequest = FundRequest
  { _fundRequest_address :: String
   -- ^ String of the address to send funds to
  , _fundRequest_amount :: Int32
  , _fundRequest_datumHash :: Maybe BS.ByteString
  }
  deriving (Generic)

type SignedTx = ByteString

instance FromJSON PaymentChannelReq
instance ToJSON PaymentChannelReq
instance FromJSON FundRequest
instance ToJSON FundRequest

instance Beam.HasColumnType PaymentChannelReq where
  defaultColumnType = const $ Beam.defaultColumnType $ Proxy @(PgJSON PaymentChannelReq)

instance FromBackendRow Postgres PaymentChannelReq where
  fromBackendRow = (\(PgJSON x) -> x) <$> fromBackendRow

instance HasSqlValueSyntax PgValueSyntax PaymentChannelReq where
  sqlValueSyntax = sqlValueSyntax . PgJSON

data PaymentChannelTaskT f = PaymentChannelTask
  { _paymentChannelTask_id :: Columnar f (SqlSerial Int32)
  , _paymentChannelTask_checkedOutBy :: Columnar f (Maybe Text)
  , _paymentChannelTask_payload :: Columnar f PaymentChannelReq
  , _paymentChannelTask_status :: Columnar f (Maybe Bool)
  , _paymentChannelTask_finished :: Columnar f Bool
  , _paymentChannelTask_time :: Columnar f UTCTime
  }
  deriving Generic

type PaymentChannelTask = PaymentChannelTaskT Identity
type PaymentChannelTaskId = PrimaryKey PaymentChannelTaskT Identity

instance Table PaymentChannelTaskT where
  newtype PrimaryKey PaymentChannelTaskT f = PaymentChannelTaskId
    { unPaymentChannelTaskId :: Columnar f (SqlSerial Int32)
    } deriving (Generic)
  primaryKey = PaymentChannelTaskId . _paymentChannelTask_id

instance Beamable PaymentChannelTaskT
instance Beamable (PrimaryKey PaymentChannelTaskT)

fmap concat $ traverse makeLenses
  [ ''PaymentChannelTaskT
  ]
