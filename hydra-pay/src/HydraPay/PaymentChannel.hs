{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PaymentChannel where

import Data.Time
import Data.Int
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Data.Foldable

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import System.IO
import System.Process
import System.Directory
import System.FilePath
import Control.Lens

import HydraPay.Proxy
import HydraPay.Logging
import HydraPay.PortRange
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra
import HydraPay.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Cardano.Api as Api
import qualified HydraPay.Database as Db

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions

data PaymentChannelConfig = PaymentChannelConfig
  { _paymentChannelConfig_name :: Text
  , _paymentChannelConfig_first :: Api.AddressAny
  , _paymentChannelConfig_second :: Api.AddressAny
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

data PaymentChannelStatus =
  PaymentChannelOpen | PaymentChannelPending
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelStatus
instance FromJSON PaymentChannelStatus

data PaymentChannelInfo = PaymentChannelInfo
  { _paymentChannelInfo_id :: Int32
  , _paymentChannelInfo_name :: Text
  , _paymentChannelInfo_other :: Text
  , _paymentChannelInfo_expiry :: UTCTime
  , _paymentChannelInfo_status :: PaymentChannelStatus
  , _paymentChannelInfo_initiator :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON PaymentChannelInfo
instance FromJSON PaymentChannelInfo

makeLenses ''PaymentChannelInfo

paymentChannelDisplayName :: PaymentChannelInfo -> Text
paymentChannelDisplayName pinfo =
  case paymentChannelCanHandle pinfo of
    True -> "New Request"
    False -> pinfo ^. paymentChannelInfo_name

paymentChannelCanHandle :: PaymentChannelInfo -> Bool
paymentChannelCanHandle pinfo =
  pinfo ^. paymentChannelInfo_status == PaymentChannelPending && not (pinfo ^. paymentChannelInfo_initiator)

getPaymentChannelsInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> m (Map Int32 PaymentChannelInfo)
getPaymentChannelsInfo a addr = do
  results <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningList $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head <- all_ (Db.db ^. Db.db_heads)
    guard_ ((paymentChannel ^. Db.paymentChannel_head) `references_` head)
    guard_ (head ^. Db.hydraHead_first ==. val_ addrStr ||. head ^. Db.hydraHead_second ==. val_ addrStr)
    pure (head, paymentChannel)
  pure $ Map.fromList $ fmap ((\p -> (p ^. paymentChannelInfo_id, p)) . (uncurry $ dbPaymentChannelToInfo addr)) results
  where
    addrStr = Api.serialiseAddress addr

getPaymentChannelInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text PaymentChannelInfo)
getPaymentChannelInfo a me pid = do
  result <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningOne $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head <- all_ (Db.db ^. Db.db_heads)
    guard_ ((paymentChannel ^. Db.paymentChannel_head) `references_` head)
    guard_ (paymentChannel ^. Db.paymentChannel_id ==. val_ (SqlSerial pid))
    pure (head, paymentChannel)
  pure $ maybeToEither "Failed to get payment channel" $ uncurry (dbPaymentChannelToInfo me) <$> result

dbPaymentChannelToInfo :: Api.AddressAny -> Db.HydraHead -> Db.PaymentChannel -> PaymentChannelInfo
dbPaymentChannelToInfo addr hh pc =
  PaymentChannelInfo
  (pc ^. Db.paymentChannel_id . to unSerial)
  (pc ^. Db.paymentChannel_name)
  other
  (pc ^. Db.paymentChannel_createTime)
  PaymentChannelPending
  isInitiator
  where
    isInitiator = addrStr == hh ^. Db.hydraHead_first

    other = case isInitiator of
      True -> hh ^. Db.hydraHead_second
      False -> hh ^. Db.hydraHead_first

    addrStr = Api.serialiseAddress addr

createPaymentChannel :: (MonadIO m, HasLogger a, HasPortRange a, HasNodeInfo a, HasHydraHeadManager a, Db.HasDbConnectionPool a) => a -> PaymentChannelConfig -> m (Either Text Int32)
createPaymentChannel a (PaymentChannelConfig name first second chain) = runExceptT $ do
  now <- liftIO $ do
    getCurrentTime

  -- Persist in database
  logInfo a "createPaymentChannel" $ "Persisting new payment channel " <> name <> " with " <> (Api.serialiseAddress first) <> " and " <> (Api.serialiseAddress second)
  dbHead <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ do
    [newHead] <- runInsertReturningList $ insert (Db.db ^. Db.db_heads) $
      insertExpressions [ Db.HydraHead
                          default_
                          (val_ firstText)
                          (val_ secondText)
                          (val_ $ chain ^. hydraChainConfig_ledgerGenesis . to T.pack)
                          (val_ $ chain ^. hydraChainConfig_ledgerProtocolParams . to T.pack)
                        ]
    runInsertReturningList $ insert (Db.db ^. Db.db_paymentChannels) $
      insertExpressions [ Db.PaymentChannel
                          default_
                          (val_ name)
                          (val_ $ primaryKey newHead)
                          (val_ now)
                        ]
    pure newHead
  nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
  hydraHead <- lift $ runHydraHead a nodeConfigs
  let
    headId = Db.hydraHeadId dbHead
  ExceptT $ trackRunningHead a headId hydraHead
  pure headId
  where
    firstText = Api.serialiseAddress first
    secondText = Api.serialiseAddress second

{-  case headMaybe results of
    Nothing -> do
      let errMsg = "Failed to create payment channel: Database persistence failed"
      logWarn a "createPaymentChannel" errMsg
      throwError errMsg
    Just dbHead -> do
      nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
      hydraHead <- ExceptT $ runHydraHead a nodeConfigs
      let
        headId = Db.paymentChannelId dbHead
      ExceptT $ trackRunningHead a headId hydraHead
      pure headId
  where
    firstText = Api.serialiseAddress first
    secondText = Api.serialiseAddress second
-}
