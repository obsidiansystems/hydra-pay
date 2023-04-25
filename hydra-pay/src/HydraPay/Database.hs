{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Database where

import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import qualified Database.Beam.AutoMigrate as BA
import qualified Cardano.Api as Api

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

data Db f = Db
  { _db_proxies :: f (TableEntity ProxiesT)
  }
  deriving (Generic)

instance Database be Db

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
