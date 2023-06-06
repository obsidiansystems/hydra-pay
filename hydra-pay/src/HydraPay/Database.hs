{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module HydraPay.Database where

import Control.Lens
import Control.Monad.IO.Class

import Data.Time
import Data.Int
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import qualified Database.Beam.AutoMigrate as Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Backend.SQL
import qualified Database.Beam.AutoMigrate as BA
import Database.PostgreSQL.Simple (withTransaction)
import Text.Read (readMaybe)

import HydraPay.Database.Workers
import HydraPay.PaymentChannel

data HydraHeadsT f = HydraHead
  { _hydraHead_id :: C f (SqlSerial Int32)
  , _hydraHead_first :: C f Text
  , _hydraHead_second :: C f Text
  , _hydraHead_firstBalance :: C f Int32
  , _hydraHead_secondBalance :: C f (Maybe Int32)
  , _hydraHead_ledgerGenesis :: C f Text
  , _hydraHead_ledgerProtocolParams :: C f Text
  }
  deriving (Generic)

instance Beamable HydraHeadsT

instance Table HydraHeadsT where
  data PrimaryKey HydraHeadsT f = HeadID (C f (SqlSerial Int32))
    deriving (Generic)
  primaryKey = HeadID . _hydraHead_id

instance Beamable (PrimaryKey HydraHeadsT)

type HydraHead = HydraHeadsT Identity
type HeadId = PrimaryKey HydraHeadsT Identity

data ProxiesT f = ProxyInfo
   { _proxy_chainAddress :: C f Text
   , _proxy_hydraAddress :: C f Text
   , _proxy_verificationKeyPath :: C f Text
   , _proxy_signingKeyPath :: C f Text
   , _proxy_hydraVerificationKeyPath :: C f Text
   , _proxy_hydraSigningKeyPath :: C f Text
   }
   deriving (Generic)

instance Beamable ProxiesT

instance Table ProxiesT where
  data PrimaryKey ProxiesT f = ProxyID (C f Text)
    deriving (Generic)
  primaryKey = ProxyID . _proxy_chainAddress

instance Beamable (PrimaryKey ProxiesT)

type ProxyInfo = ProxiesT Identity


instance BA.HasColumnType PaymentChannelStatus where
  defaultColumnType = const $ Beam.defaultColumnType $ Proxy @Text

instance FromBackendRow Postgres PaymentChannelStatus where
  fromBackendRow = do
    row <- fromBackendRow
    case readMaybe row of
      Just x -> pure x
      Nothing -> fail $ "Unknown PaymentChannelStatus: " <> row

instance HasSqlValueSyntax PgValueSyntax PaymentChannelStatus where
  sqlValueSyntax = sqlValueSyntax . T.pack . show

instance HasSqlEqualityCheck Postgres PaymentChannelStatus


data PaymentChannelsT f = PaymentChannel
  { _paymentChannel_id :: C f (SqlSerial Int32)
  , _paymentChannel_name :: C f Text
  , _paymentChannel_head :: PrimaryKey HydraHeadsT f
  , _paymentChannel_createdAt :: C f UTCTime
  , _paymentChannel_expiry :: C f UTCTime
  , _paymentChannel_status :: C f PaymentChannelStatus
  }
  deriving (Generic)

instance Beamable PaymentChannelsT

instance Table PaymentChannelsT where
  data PrimaryKey PaymentChannelsT f = PaymentChannelID (C f (SqlSerial Int32))
    deriving (Generic)
  primaryKey = PaymentChannelID . _paymentChannel_id

instance Beamable (PrimaryKey PaymentChannelsT)

type PaymentChannel = PaymentChannelsT Identity

data TransactionsT f = Transaction
  { _transaction_id :: C f (SqlSerial Int32)
  , _transaction_head :: PrimaryKey HydraHeadsT f
  , _transaction_party :: C f Text
  , _transaction_time :: C f UTCTime
  , _transaction_amount :: C f Int32
  }
  deriving (Generic)

instance Beamable TransactionsT

instance Table TransactionsT where
  data PrimaryKey TransactionsT f = TransactionID (C f (SqlSerial Int32))
    deriving (Generic)
  primaryKey = TransactionID . _transaction_id

instance Beamable (PrimaryKey TransactionsT)

type Transaction = TransactionsT Identity

data Db f = Db
  { _db_proxies :: f (TableEntity ProxiesT)
  , _db_heads :: f (TableEntity HydraHeadsT)
  , _db_paymentChannels :: f (TableEntity PaymentChannelsT)
  , _db_transactions :: f (TableEntity TransactionsT)
  , _db_openChanTask :: f (TableEntity OpenChannelTaskT)
  }
  deriving (Generic)

instance Database be Db

class HasDbConnectionPool a where
  dbConnectionPool :: Lens' a (Pool Connection)

instance HasDbConnectionPool (Pool Connection) where
  dbConnectionPool = id

makeLenses ''PaymentChannelsT
makeLenses ''TransactionsT
makeLenses ''HydraHeadsT
makeLenses ''ProxiesT
makeLenses ''Db

hydraHeadId :: HydraHead -> Int32
hydraHeadId = unSerial . _hydraHead_id

paymentChannelId :: PaymentChannel -> Int32
paymentChannelId = unSerial . _paymentChannel_id

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

annotatedDb :: BA.AnnotatedDatabaseSettings Postgres Db
annotatedDb = BA.defaultAnnotatedDbSettings db

schema :: BA.Schema
schema = BA.fromAnnotatedDbSettings annotatedDb (Proxy @'[])

doAutomigrate :: Connection -> IO ()
doAutomigrate =
  BA.tryRunMigrationsWithEditUpdate annotatedDb

runQueryInTransaction :: (HasDbConnectionPool a, MonadIO m) => a -> (Connection -> IO b) -> m b
runQueryInTransaction a action = liftIO $ withResource pool $ \conn -> do
  withTransaction conn (action conn)
  where
    pool = a ^. dbConnectionPool

runBeam :: (HasDbConnectionPool a, MonadIO m) => a -> Pg b -> m b
runBeam a action = runQueryInTransaction a $ \conn -> runBeamPostgresDebug putStrLn conn action
