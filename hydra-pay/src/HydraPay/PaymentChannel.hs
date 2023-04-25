{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PaymentChannel where

import Data.Int
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Data.Foldable

import Control.Monad.IO.Class
import Control.Monad.Error.Class
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
  { _paymentChannelManager_runningChannels :: TMVar (Map Int32 (TMVar HydraHead))
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
  -- , paymentChannelInfo_expiry :: UTCTime
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

withPaymentChannelManager :: (PaymentChannelManager -> IO a) -> IO a
withPaymentChannelManager action = do
  bracket newPaymentChannelManager terminateRunningPaymentChannels action

newPaymentChannelManager :: MonadIO m => m PaymentChannelManager
newPaymentChannelManager = PaymentChannelManager <$> (liftIO . newTMVarIO) mempty

terminateRunningPaymentChannels :: MonadIO m => PaymentChannelManager -> m ()
terminateRunningPaymentChannels (PaymentChannelManager channels) = do
  withTMVar channels $ \running -> do
    for_ (Map.elems running) $ \headVar -> do
      withTMVar headVar $ \hydraHead@(HydraHead handles) -> do
        for_ handles $ \(HydraNode _ nodeHandle@(_, out, err, _) thread _ _ _) -> liftIO $ do
          cleanupProcess nodeHandle
          maybe (pure ()) hClose out
          maybe (pure ()) hClose err
          killThread thread
        pure (hydraHead, ())
    pure (running, ())

getRunningHead :: (MonadIO m, HasLogger a, HasPaymentChannelManager a) => a -> Int32 -> m (Either Text (TMVar HydraHead))
getRunningHead a hid = do
  logInfo a "getRunningHead" $ "Fetching head " <> tShow hid
  liftIO $ withTMVar runningChannels $ \running -> do
    case Map.lookup hid running of
      Just h -> pure (running, Right h)
      Nothing -> pure (running, Left $ "Running head with id " <> tShow hid <> " couldn't be found")
  where
    runningChannels = a ^. paymentChannelManager . paymentChannelManager_runningChannels

trackRunningHead :: (MonadIO m, HasLogger a, HasPaymentChannelManager a) => a -> Int32 -> HydraHead -> m (Either Text ())
trackRunningHead a hid hydraHead = do
  logInfo a "addRunningHeadToManager" $ "Adding a running head to the payment channel manager"
  liftIO $ withTMVar runningChannels $ \running -> do
    case Map.lookup hid running of
      Just _ -> do
        let errMsg = "Running head already found under" <> tShow hid
        logWarn a "addRunningHeadToManager" errMsg
        pure (running, Left errMsg)
      Nothing -> do
        logInfo a "addRunningHeadToManager" $ "New head added to payment channel manager under " <> tShow hid
        headVar <- newTMVarIO hydraHead
        pure (Map.insert hid headVar running, Right ())
  where
    runningChannels = a ^. paymentChannelManager . paymentChannelManager_runningChannels

initializePaymentChannel :: (MonadIO m, HasLogger a, HasPaymentChannelManager a) => a -> Int32 -> m (Either Text ())
initializePaymentChannel a pid = do
  logInfo a "initializePaymentChannel" $ "Attempting to Init channel " <> tShow pid
  result <- runExceptT $ do
    hydraHeadVar <- ExceptT $ getRunningHead a pid
    ExceptT $ liftIO $ withTMVar hydraHeadVar $ \hydraHead -> do
      result <- sendHydraHeadCommand hydraHead $ initHydraHead
      pure(hydraHead, result)
  case result of
    Right _ -> logInfo a "initializePaymentChannel" $ "Payment channel " <> tShow pid <> " is initialized"
    Left err -> logInfo a "initializePaymentChannel" $ "Payment channel failed to initialize: " <> err
  pure result

getPaymentChannelsInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> m (Map Int32 PaymentChannelInfo)
getPaymentChannelsInfo a addr = do
  results <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningList $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    guard_ (paymentChannel ^. Db.paymentChannel_first ==. val_ addrStr ||. paymentChannel ^. Db.paymentChannel_second ==. val_ addrStr)
    pure paymentChannel
  pure $ Map.fromList $ fmap ((\x -> (x ^. paymentChannelInfo_id, x)) . dbPaymentChannelToInfo addr) results
  where
    addrStr = Api.serialiseAddress addr

getPaymentChannelInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text PaymentChannelInfo)
getPaymentChannelInfo a me pid = do
  result <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningOne $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    guard_ (paymentChannel ^. Db.paymentChannel_id ==. val_ (SqlSerial pid))
    pure paymentChannel
  pure $ maybeToEither "Failed to get payment channel" $ dbPaymentChannelToInfo me <$> result

dbPaymentChannelToInfo :: Api.AddressAny -> Db.PaymentChannel -> PaymentChannelInfo
dbPaymentChannelToInfo addr pc =
  PaymentChannelInfo
  (pc ^. Db.paymentChannel_id . to unSerial)
  (pc ^. Db.paymentChannel_name)
  other
  PaymentChannelPending
  isInitiator
  where
    isInitiator = addrStr == pc ^. Db.paymentChannel_first

    other = case isInitiator of
      True -> pc ^. Db.paymentChannel_second
      False -> pc ^. Db.paymentChannel_first

    addrStr = Api.serialiseAddress addr

createPaymentChannel :: (MonadIO m, HasLogger a, HasPortRange a, HasNodeInfo a, HasPaymentChannelManager a, Db.HasDbConnectionPool a) => a -> PaymentChannelConfig -> m (Either Text Int32)
createPaymentChannel a (PaymentChannelConfig name first second chain) = runExceptT $ do
  liftIO $ do
    createDirectoryIfMissing True paymentChannelNodePersistenceDir
    createDirectoryIfMissing True paymentChannelNodeLogsDir

  -- Persist in database
  logInfo a "createPaymentChannel" $ "Persisting new payment channel " <> name <> " with " <> (Api.serialiseAddress first) <> " and " <> (Api.serialiseAddress second)
  results <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runInsertReturningList $ insert (Db.db ^. Db.db_paymentChannels) $
    insertExpressions [ Db.PaymentChannel
                        default_
                        (val_ name)
                        (val_ $ firstText)
                        (val_ $ secondText)
                      ]
  case headMaybe results of
    Nothing -> do
      let errMsg = "Failed to create payment channel: Database persistence failed"
      logWarn a "createPaymentChannel" errMsg
      throwError errMsg
    Just dbChan -> do
      -- On success we startup the Head and make sure the manager is aware of it
      firstProxy <- ExceptT $ queryProxyInfo a first
      secondProxy <- ExceptT $ queryProxyInfo a second
      let
        suffixFirst = (T.unpack $ name <> "-" <> T.takeEnd 8 firstText)
        suffixSecond = (T.unpack $ name <> "-" <> T.takeEnd 8 secondText)
        persistFirst = paymentChannelNodePersistenceDir </> suffixFirst
        persistSecond = paymentChannelNodePersistenceDir </> suffixSecond
        logFirst = paymentChannelNodeLogsDir </> (suffixFirst <> ".log")
        logSecond = paymentChannelNodeLogsDir </> (suffixSecond <> ".log")
        logErrFirst = paymentChannelNodeLogsDir </> (suffixFirst <> ".error.log")
        logErrSecond = paymentChannelNodeLogsDir </> (suffixSecond <> ".error.log")
        tlogFirst = paymentChannelNodeLogsDir </> (suffixFirst <> ".thread.log")
        tlogSecond = paymentChannelNodeLogsDir </> (suffixSecond <> ".thread.log")

        headConfig = TwoPartyHeadConfig
          firstProxy
          secondProxy
          persistFirst
          persistSecond
          logFirst
          logSecond
          logErrFirst
          logErrSecond
          tlogFirst
          tlogSecond

      liftIO $ do
        createDirectoryIfMissing True persistFirst
        createDirectoryIfMissing True persistSecond

      nodeConfigs <- ExceptT $ mkTwoPartyHydraNodeConfigs a previewScriptTxId (a ^. portRange) chain headConfig
      hydraHead <- runHydraHead a nodeConfigs
      let
        headId = Db.paymentChannelId dbChan
      ExceptT $ trackRunningHead a headId hydraHead
      pure headId
  where
    firstText = Api.serialiseAddress first
    secondText = Api.serialiseAddress second

paymentChannelNodePersistenceDir :: FilePath
paymentChannelNodePersistenceDir = "payment-channel-persistence"

paymentChannelNodeLogsDir :: FilePath
paymentChannelNodeLogsDir = "payment-channel-logs"
