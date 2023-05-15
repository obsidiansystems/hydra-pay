{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PaymentChannel where

import Data.Time
import Data.Int
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import HydraPay.Logging
import HydraPay.Cardano.Hydra
import HydraPay.Utils

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Cardano.Api as Api
import qualified HydraPay.Database as Db

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgExpressionSyntax(..), emit)
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions

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

getPaymentChannelsInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> m (Map Int32 PaymentChannelInfo)
getPaymentChannelsInfo a addr = do
  results <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningList $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head_ <- join_ (Db.db ^. Db.db_heads) (\head_ -> (paymentChannel ^. Db.paymentChannel_head) `references_` head_)
    guard_ (paymentChannel ^. Db.paymentChannel_open &&. (head_ ^. Db.hydraHead_first ==. val_ addrStr ||. head_ ^. Db.hydraHead_second ==. val_ addrStr))
    pure (head_, paymentChannel)
  pure $ Map.fromList $ fmap ((\p -> (p ^. paymentChannelInfo_id, p)) . (uncurry $ dbPaymentChannelToInfo addr)) results
  where
    addrStr = Api.serialiseAddress addr

getPaymentChannelInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text PaymentChannelInfo)
getPaymentChannelInfo a me pid = do
  result <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningOne $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head_ <- all_ (Db.db ^. Db.db_heads)
    guard_ ((paymentChannel ^. Db.paymentChannel_head) `references_` head_)
    guard_ (paymentChannel ^. Db.paymentChannel_id ==. val_ (SqlSerial pid))
    pure (head_, paymentChannel)
  pure $ maybeToEither "Failed to get payment channel" $ uncurry (dbPaymentChannelToInfo me) <$> result

dbPaymentChannelToInfo :: Api.AddressAny -> Db.HydraHead -> Db.PaymentChannel -> PaymentChannelInfo
dbPaymentChannelToInfo addr hh pc =
  PaymentChannelInfo
  (pc ^. Db.paymentChannel_id . to unSerial)
  (pc ^. Db.paymentChannel_name)
  other
  status
  isInitiator
  where
    status =
      case hh ^. Db.hydraHead_secondBalance of
        Just _ -> PaymentChannelOpen
        Nothing -> (PaymentChannelPending $ pc ^. Db.paymentChannel_expiry)

    isInitiator = addrStr == hh ^. Db.hydraHead_first

    other = case isInitiator of
      True -> hh ^. Db.hydraHead_second
      False -> hh ^. Db.hydraHead_first

    addrStr = Api.serialiseAddress addr

getPaymentChannelDetails :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text (Int32, Map Int32 TransactionInfo))
getPaymentChannelDetails a addr hid = do
  Db.runBeam a $ runExceptT $ do
    mHead <- runSelectReturningOne $ select $ do
      h <- all_ (Db.db ^. Db.db_heads)
      guard_ (h ^. Db.hydraHead_id ==. val_ (SqlSerial hid))
      pure h

    case mHead of
      Nothing -> throwError $ "Invalid head " <> tShow hid
      Just h -> do
        let
          isFirst = h ^. Db.hydraHead_first == Api.serialiseAddress addr

          currentBalance =
            case isFirst of
              True -> h ^. Db.hydraHead_firstBalance
              False -> maybe (error "Invalid payment channel") id $ h ^. Db.hydraHead_secondBalance

        transactions <- runSelectReturningList $ select $ do
          ts <- all_ (Db.db ^. Db.db_transactions)
          guard_ (ts ^. Db.transaction_head ==. val_ (primaryKey h))
          pure ts

        let
          infos = Map.fromList $ fmap ((\x -> (x ^. transactionInfo_id, x)) . (dbTransactionToTransactionInfo addr)) transactions

        pure (currentBalance, infos)

-- | Retrieve the total balance locked in hydra heads filtering ones that have expired.
getHydraBalanceSlow :: (MonadBeam Postgres m) => Api.AddressAny -> m (Either Text Api.Lovelace)
getHydraBalanceSlow addr = do
  let addrText = Api.serialiseAddress addr
  mbalanceFirst <- runSelectReturningOne $ select $ fmap snd $
    aggregate_ (\h -> (group_ (Db._hydraHead_first h), sum_ (h ^. Db.hydraHead_firstBalance))) $ do
      heads_ <- all_ (Db.db ^. Db.db_heads)
      paymentChan <- join_ (Db.db ^. Db.db_paymentChannels) (\paymentChan -> (paymentChan ^. Db.paymentChannel_head) `references_` heads_)
      guard_ (heads_ ^. Db.hydraHead_first ==. val_ addrText &&. paymentChan ^. Db.paymentChannel_open)
      pure heads_
  mbalanceSecond <- runSelectReturningOne $ select $ fmap snd $
    aggregate_ (\h -> (group_ (Db._hydraHead_second h), sum_ (fromMaybe_ 0 $ h ^. Db.hydraHead_secondBalance))) $ do
      heads_ <- all_ (Db.db ^. Db.db_heads)
      paymentChan <- join_ (Db.db ^. Db.db_paymentChannels) (\paymentChan -> (paymentChan ^. Db.paymentChannel_head) `references_` heads_)
      guard_ (heads_ ^. Db.hydraHead_second ==. val_ addrText &&. paymentChan ^. Db.paymentChannel_open)
      pure heads_
  pure $ Right $ mconcat
    [ fromIntegral $ fromMaybe 0 (join mbalanceFirst)
    , fromIntegral $ fromMaybe 0 (join mbalanceSecond)
    ]

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
current_timestamp_ :: QExpr Postgres s UTCTime
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp"))

addOneDayInterval_ :: QExpr Postgres s UTCTime -> QExpr Postgres s UTCTime
addOneDayInterval_ (QExpr e) = QExpr (addE <$> e <*> pure (PgExpressionSyntax $ emit "interval '1 day'"))

dbTransactionToTransactionInfo :: Api.AddressAny -> Db.Transaction -> TransactionInfo
dbTransactionToTransactionInfo addr t =
  TransactionInfo
  (t ^. Db.transaction_id . to unSerial)
  (t ^. Db.transaction_time)
  (t ^. Db.transaction_amount)
  (if Api.serialiseAddress addr == (t ^. Db.transaction_party)
   then TransactionSent
   else TransactionReceived
  )

sendAdaInChannel :: (MonadIO m, MonadBeamInsertReturning Postgres m) => Int32 -> Api.AddressAny -> Int32 -> m (Either Text (Int32, TransactionInfo))
sendAdaInChannel hid you amount = runExceptT $ do
    mHead <- runSelectReturningOne $ select $ do
      h <- all_ (Db.db ^. Db.db_heads)
      guard_ (h ^. Db.hydraHead_id ==. val_ (SqlSerial hid))
      pure h

    case mHead of
      Nothing -> throwError $ "Invalid head " <> tShow hid
      Just h -> do
        let
          isFirst = h ^. Db.hydraHead_first == Api.serialiseAddress you

          newBalance =
            case isFirst of
              True -> h ^. Db.hydraHead_firstBalance - amount
              False -> maybe (error "Invalid payment channel") (subtract amount) $ h ^. Db.hydraHead_secondBalance

        runUpdate $
          update (Db.db ^. Db.db_heads)
          (case isFirst of
             True ->
               (\channel -> mconcat [ channel ^. Db.hydraHead_firstBalance <-. val_ (h ^. Db.hydraHead_firstBalance - amount)
                                    , channel ^. Db.hydraHead_secondBalance <-. val_ (fmap (+ amount) $ h ^. Db.hydraHead_secondBalance)
                                    ]
               )
             False ->
               (\channel -> mconcat [ channel ^. Db.hydraHead_secondBalance <-. val_ (fmap (subtract amount) $ h ^. Db.hydraHead_secondBalance)
                                    , channel ^. Db.hydraHead_firstBalance <-. val_ (h ^. Db.hydraHead_firstBalance + amount)
                                    ]
               )
             )
          (\channel -> channel ^. Db.hydraHead_id ==. val_ (SqlSerial hid))

        results <- runInsertReturningList $ insert (Db.db ^. Db.db_transactions) $
          insertExpressions [ Db.Transaction
                              default_
                              (val_ $ primaryKey h)
                              (val_ $ Api.serialiseAddress you)
                              current_timestamp_
                              (val_ amount)
                            ]
        pure (newBalance, dbTransactionToTransactionInfo you $ head results)

joinPaymentChannel :: (MonadBeam Postgres m, MonadFail m) => Int32 -> Int32 -> m ()
joinPaymentChannel hid amount = do
  -- TODO(skylar): Do we 'try' to get a useful error message here?
    Just paymentChannel <- runSelectReturningOne (lookup_ (Db.db ^. Db.db_paymentChannels) (Db.PaymentChannelID $ SqlSerial hid))
    let Db.HeadID headId = paymentChannel ^. Db.paymentChannel_head
    runUpdate $
      update
      (Db.db ^. Db.db_heads)
      (\channel -> channel ^. Db.hydraHead_secondBalance <-. val_ (Just amount))
      (\channel -> channel ^. Db.hydraHead_id ==. val_ headId)

createPaymentChannel :: (MonadIO m, MonadFail m, MonadBeamInsertReturning Postgres m, HasLogger a) => a -> PaymentChannelConfig -> m Int32
createPaymentChannel a (PaymentChannelConfig name first second amount chain) = do
  -- Persist in database
  logInfo a "createPaymentChannel" $ "Persisting new payment channel " <> name <> " with " <> (Api.serialiseAddress first) <> " and " <> (Api.serialiseAddress second)
  dbHead <- do
    [newHead] <- runInsertReturningList $ insert (Db.db ^. Db.db_heads) $
      insertExpressions [ Db.HydraHead
                          default_
                          (val_ firstText)
                          (val_ secondText)
                          (val_ amount)
                          (val_ Nothing)
                          (val_ $ chain ^. hydraChainConfig_ledgerGenesis . to T.pack)
                          (val_ $ chain ^. hydraChainConfig_ledgerProtocolParams . to T.pack)
                        ]
    _ <- runInsertReturningList $ insert (Db.db ^. Db.db_paymentChannels) $
      insertExpressions [ Db.PaymentChannel
                          default_
                          (val_ name)
                          (val_ $ primaryKey newHead)
                          (addOneDayInterval_ current_timestamp_)
                          (val_ True)
                        ]
    pure newHead
  -- TODO(skylar): Should creation imply spinning up nodes etc? I don't think so
  -- nodeConfigs <- ExceptT $ deriveConfigFromDbHead a dbHead
  -- hydraHead <- lift $ runHydraHead a nodeConfigs
  -- let
  --   headId = Db.hydraHeadId dbHead
  -- ExceptT $ trackRunningHead a headId hydraHead
  pure $ Db.hydraHeadId dbHead
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

destroyPaymentChannel :: (MonadIO m, HasLogger a, Db.HasDbConnectionPool a) => a -> Int32 -> m (Either Text ())
destroyPaymentChannel a headId = runExceptT $ do
  logInfo a "destroyPaymentChannel" $ "Closing head with id: " <> tShow headId
  Db.runBeam a $ closeChannelQ headId

closeChannelQ :: (MonadBeam Postgres m) => Int32 -> m ()
closeChannelQ headId = do
  runUpdate $ update (Db.db ^. Db.db_paymentChannels)
    (\channel -> channel ^. Db.paymentChannel_open <-. val_ False)
    (\channel -> channel ^. Db.paymentChannel_head ==. val_ (Db.HeadID (SqlSerial headId)))
