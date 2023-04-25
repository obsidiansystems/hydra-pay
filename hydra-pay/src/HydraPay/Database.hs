{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Database where

import Control.Lens
import Control.Monad.IO.Class

import Data.Int
import Data.Pool
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import qualified Database.Beam.AutoMigrate as BA
import Database.PostgreSQL.Simple (withTransaction)

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

data PaymentChannelsT f = PaymentChannel
  { _paymentChannel_id :: C f (SqlSerial Int32)
  , _paymentChannel_name :: C f Text
  , _paymentChannel_first :: C f Text
  , _paymentChannel_second :: C f Text
  }
  deriving (Generic)

instance Beamable PaymentChannelsT

instance Table PaymentChannelsT where
  data PrimaryKey PaymentChannelsT f = PaymentChannelID (C f (SqlSerial Int32))
    deriving (Generic)
  primaryKey = PaymentChannelID . _paymentChannel_id

instance Beamable (PrimaryKey PaymentChannelsT)

type PaymentChannel = PaymentChannelsT Identity

makeLenses ''PaymentChannelsT

paymentChannelId :: PaymentChannel -> Int32
paymentChannelId = unSerial . _paymentChannel_id

data Db f = Db
  { _db_proxies :: f (TableEntity ProxiesT)
  , _db_paymentChannels :: f (TableEntity PaymentChannelsT)
  }
  deriving (Generic)

instance Database be Db

class HasDbConnectionPool a where
  dbConnectionPool :: Lens' a (Pool Connection)

instance HasDbConnectionPool (Pool Connection) where
  dbConnectionPool = id

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

annotatedDb :: BA.AnnotatedDatabaseSettings Postgres Db
annotatedDb = BA.defaultAnnotatedDbSettings db

schema :: BA.Schema
schema = BA.fromAnnotatedDbSettings annotatedDb (Proxy @'[])

doAutomigrate :: Connection -> IO ()
doAutomigrate =
  BA.tryRunMigrationsWithEditUpdate annotatedDb

makeLenses ''ProxiesT
makeLenses ''Db

runQueryInTransaction :: (HasDbConnectionPool a, MonadIO m) => a -> (Connection -> IO b) -> m b
runQueryInTransaction a action = liftIO $ withResource pool $ \conn -> do
  withTransaction conn (action conn)
  where
    pool = a ^. dbConnectionPool
