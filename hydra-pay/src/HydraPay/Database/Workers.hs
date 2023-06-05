-- | Tracking Hydra Tasks from a database.

{-# LANGUAGE TemplateHaskell #-}
module HydraPay.Database.Workers where

import Control.Lens (iso, makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Pool
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Database.Beam
import qualified Database.Beam.AutoMigrate as Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax, PgExpressionSyntax(..), emit)
import Database.PostgreSQL.Simple.FromField

import Obelisk.Route
import Rhyolite.DB.Beam.Types
import Rhyolite.Task.Beam
import Rhyolite.Task.Beam.Worker


data OpenChannelReq = OpenChannelReq
  { _openChannelReq_headId :: Int32
  -- ^ The head id used to find the data required on the db to fullfill the Open
  -- Channel request.
  }
  deriving Generic

instance FromJSON OpenChannelReq
instance ToJSON OpenChannelReq

instance Beam.HasColumnType OpenChannelReq where
  defaultColumnType = const $ Beam.defaultColumnType $ Proxy @(PgJSON OpenChannelReq)

instance FromBackendRow Postgres OpenChannelReq where
  fromBackendRow = (\(PgJSON x) -> x) <$> fromBackendRow

instance HasSqlValueSyntax PgValueSyntax OpenChannelReq where
  sqlValueSyntax = sqlValueSyntax . PgJSON


data OpenChannelTaskT f = OpenChannelTask
  { _openChannelTask_id :: Columnar f (SqlSerial Int32)
  , _openChannelTask_checkedOutBy :: Columnar f (Maybe Text)
  , _openChannelTask_payload :: Columnar f OpenChannelReq
  , _openChannelTask_status :: Columnar f (Maybe Bool)
  , _openChannelTask_finished :: Columnar f Bool
  , _openChannelTask_time :: Columnar f UTCTime
  }
  deriving Generic

type OpenChannelTask = OpenChannelTaskT Identity
type OpenChannelTaskId = PrimaryKey OpenChannelTaskT Identity

instance Table OpenChannelTaskT where
  newtype PrimaryKey OpenChannelTaskT f = OpenChannelTaskId
    { unOpenChannelTaskId :: Columnar f (SqlSerial Int32)
    } deriving (Generic)
  primaryKey = OpenChannelTaskId . _openChannelTask_id

instance Beamable OpenChannelTaskT
instance Beamable (PrimaryKey OpenChannelTaskT)

fmap concat $ traverse makeLenses
  [ ''OpenChannelTaskT
  ]
