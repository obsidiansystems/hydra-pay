{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PaymentChannel.Postgres where

import Data.Time
import Data.Int
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import qualified Data.Text as T

import Control.Lens hiding ((<.))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import HydraPay.Logging
import HydraPay.PaymentChannel
import HydraPay.Cardano.Hydra.ChainConfig
import HydraPay.Cardano.Hydra.Status
import HydraPay.Database.Workers (RefundRequest(..))
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

getPaymentChannelsInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> m (Map Int32 PaymentChannelInfo)
getPaymentChannelsInfo a addr = Db.runBeam a $ do
  channels :: [(Db.HydraHead, Db.PaymentChannel)] <- runSelectReturningList $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head_ <- all_ (Db.db ^. Db.db_heads)
    guard_ ((paymentChannel ^. Db.paymentChannel_head) `references_` head_)
    guard_ (head_ ^. Db.hydraHead_status /=. val_ HydraHeadStatus_Done &&. (head_ ^. Db.hydraHead_first ==. val_ addrStr ||. head_ ^. Db.hydraHead_second ==. val_ addrStr))
    pure (head_, paymentChannel)

  fmap mconcat $ for channels $ \(head_, chan) -> do
    commits <- runSelectReturningList $ select $ do
      observedCommits_ <- all_ (Db.db ^. Db.db_observedCommits)
      guard_ (observedCommits_ ^. Db.observedCommit_head ==. (val_ $ Db.HeadId $ head_ ^. Db.hydraHead_id))
      pure observedCommits_
    let
      pinfo = dbPaymentChannelToInfo addr head_ chan commits
      -- head_ chan commits 
    pure $ Map.singleton (pinfo ^. paymentChannelInfo_id) pinfo 

      where
    addrStr = Api.serialiseAddress addr

getPaymentChannelInfo :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text PaymentChannelInfo)
getPaymentChannelInfo a me pid = do
  Db.runBeam a $ runExceptT $ do
    (head_, pc) <- ExceptT $ fmap (maybeToEither "Failed to get payment channel") $ runSelectReturningOne $ select $ do
      paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
      head_ <- all_ (Db.db ^. Db.db_heads)
      guard_ ((paymentChannel ^. Db.paymentChannel_head) `references_` head_)
      guard_ (paymentChannel ^. Db.paymentChannel_id ==. val_ (SqlSerial pid))
      pure (head_, paymentChannel)
    commits <- runSelectReturningList $ select $ do
      observedCommits_ <- all_ (Db.db ^. Db.db_observedCommits)
      guard_ (observedCommits_ ^. Db.observedCommit_head ==. (val_ $ Db.HeadId $ head_ ^. Db.hydraHead_id))
      pure observedCommits_
    pure $ dbPaymentChannelToInfo me head_ pc commits 

dbPaymentChannelToInfo :: Api.AddressAny -> Db.HydraHead -> Db.PaymentChannel -> [Db.ObservedCommit]  -> PaymentChannelInfo
dbPaymentChannelToInfo addr hh pc _ =
  PaymentChannelInfo
    { _paymentChannelInfo_id = pc ^. Db.paymentChannel_id . to unSerial
    , _paymentChannelInfo_name = pc ^. Db.paymentChannel_name
    , _paymentChannelInfo_createdAt = pc ^. Db.paymentChannel_createdAt
    , _paymentChannelInfo_expiry = pc ^. Db.paymentChannel_expiry
    , _paymentChannelInfo_other = other
    , _paymentChannelInfo_status = pc ^. Db.paymentChannel_status
    , _paymentChannelInfo_initiator = isInitiator
    , _paymentChannelInfo_balance = balance
    }
  where
    isInitiator = addrStr == hh ^. Db.hydraHead_first

    other = case isInitiator of
      True -> hh ^. Db.hydraHead_second
      False -> hh ^. Db.hydraHead_first

    addrStr = Api.serialiseAddress addr

    balance :: Maybe Api.Lovelace
    balance =
      let selfBalance = if isInitiator then hh ^. Db.hydraHead_firstBalance else fromMaybe 0 $ hh ^. Db.hydraHead_secondBalance
      in fromIntegral <$> case pc ^. Db.paymentChannel_status of
        PaymentChannelStatus_Submitting ->
          if isInitiator then Just (hh ^. Db.hydraHead_firstBalance) else Nothing
        PaymentChannelStatus_WaitingForAccept ->
          if isInitiator then Just (hh ^. Db.hydraHead_firstBalance) else Nothing
        PaymentChannelStatus_Opening ->
          Just selfBalance
        PaymentChannelStatus_Open ->
          Just selfBalance
        PaymentChannelStatus_Closing ->
          Just selfBalance
        PaymentChannelStatus_Error ->
          Nothing

getPaymentChannelDetails :: (MonadIO m, Db.HasDbConnectionPool a) => a -> Api.AddressAny -> Int32 -> m (Either Text (Int32, Map Int32 TransactionInfo))
getPaymentChannelDetails a addr pcId = do
  Db.runBeam a $ runExceptT $ do
    mHead <- runSelectReturningOne $ select $ do
      heads_ <- all_ (Db.db ^. Db.db_heads)
      paymentChan_ <- join_ (Db.db ^. Db.db_paymentChannels) (\paymentChan -> (paymentChan ^. Db.paymentChannel_head) `references_` heads_)
      guard_ (paymentChan_ ^. Db.paymentChannel_id ==. val_ (SqlSerial pcId))
      pure heads_

    case mHead of
      Nothing -> throwError $ "Invalid channel " <> tShow pcId
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

-- | Retrieves payment channels requests that have expired along with address and balance information
getExpiredPaymentChannels :: (MonadIO m, Db.HasDbConnectionPool a) => a -> UTCTime -> m ([RefundRequest])
getExpiredPaymentChannels a now = do
  results <- Db.runQueryInTransaction a $ \conn -> runBeamPostgres conn $ runSelectReturningList $ select $ do
    paymentChannel <- all_ (Db.db ^. Db.db_paymentChannels)
    head_ <- join_ (Db.db ^. Db.db_heads) (\head_ -> (paymentChannel ^. Db.paymentChannel_head) `references_` head_)
    proxies <- join_ (Db.db ^. Db.db_proxies) (\proxy -> (head_ ^. Db.hydraHead_first) ==. (proxy ^. Db.proxy_chainAddress))
    guard_ (paymentChannel ^. Db.paymentChannel_status <. val_ PaymentChannelStatus_Opening &&. paymentChannel ^. Db.paymentChannel_expiry Database.Beam.<. val_ now)
    pure (head_, paymentChannel, proxies)
  forM results $ \(head', _, prox) -> return $ RefundRequest
    { _refundRequest_hydraHead = unSerial $ Db._hydraHead_id head'
    , _refundRequest_hydraAddress = Db._proxy_hydraAddress prox
    , _refundRequest_signingKeyPath = T.unpack $ Db._proxy_signingKeyPath prox
    , _refundRequest_chainAddress = Db._proxy_chainAddress prox
    , _refundRequest_amount = Db._hydraHead_firstBalance head'
    , _refundRequest_protocolParams = Db._hydraHead_ledgerProtocolParams head'
    }

-- | Retrieve the total balance locked in hydra heads filtering ones that have expired.
getHydraBalanceSlow :: (MonadBeam Postgres m) => Api.AddressAny -> m (Either Text Api.Lovelace)
getHydraBalanceSlow addr = do
  let addrText = Api.serialiseAddress addr
  mbalanceFirst <- runSelectReturningOne $ select $ fmap snd $
    aggregate_ (\h -> (group_ (Db._hydraHead_first h), sum_ (h ^. Db.hydraHead_firstBalance))) $ do
      head_ <- all_ (Db.db ^. Db.db_heads)
      guard_ (head_ ^. Db.hydraHead_first ==. val_ addrText &&. head_ ^. Db.hydraHead_status /=. val_ HydraHeadStatus_Done)
      pure head_
  mbalanceSecond <- runSelectReturningOne $ select $ fmap snd $
    aggregate_ (\h -> (group_ (Db._hydraHead_second h), sum_ (fromMaybe_ 0 $ h ^. Db.hydraHead_secondBalance))) $ do
      head_ <- all_ (Db.db ^. Db.db_heads)
      guard_ (head_ ^. Db.hydraHead_second ==. val_ addrText &&. head_ ^. Db.hydraHead_status /=. val_ HydraHeadStatus_Done)
      pure head_
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

getPaymentChannelHeadId :: (MonadBeam Postgres m, MonadFail m) => Int32 -> m Int32
getPaymentChannelHeadId pcId = do
  Just paymentChannel <- runSelectReturningOne (lookup_ (Db.db ^. Db.db_paymentChannels) (Db.PaymentChannelId $ SqlSerial pcId))
  let Db.HeadId (SqlSerial hid) = Db._paymentChannel_head paymentChannel
  pure hid

getHydraHead :: (MonadBeam Postgres m, MonadFail m) => Int32 -> m Db.HydraHead
getHydraHead headId = do
  Just hydraHead <- runSelectReturningOne (lookup_ (Db.db ^. Db.db_heads) (Db.HeadId $ SqlSerial headId))
  pure hydraHead

joinPaymentChannel :: (MonadBeam Postgres m) => Int32 -> Int32 -> m ()
joinPaymentChannel headId amount = do
  -- TODO(skylar): Do we 'try' to get a useful error message here?
    runUpdate $
      update
      (Db.db ^. Db.db_heads)
      (\channel -> channel ^. Db.hydraHead_secondBalance <-. val_ (Just amount))
      (\channel -> channel ^. Db.hydraHead_id ==. val_ (SqlSerial headId))

createPaymentChannel :: (MonadIO m, MonadFail m, MonadBeamInsertReturning Postgres m, HasLogger a) => a -> PaymentChannelConfig -> m Db.HeadId
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
                          (val_ Nothing)
                          (val_ HydraHeadStatus_Created)
                          (val_ False)
                        ]
    _ <- runInsertReturningList $ insert (Db.db ^. Db.db_paymentChannels) $
      insertExpressions [ Db.PaymentChannel
                          default_
                          (val_ name)
                          (val_ $ primaryKey newHead)
                          current_timestamp_
                          (addOneDayInterval_ current_timestamp_)
                          (val_ PaymentChannelStatus_Submitting)
                          (val_ 0)
                          (val_ False)
                        ]
    pure newHead
  pure $ primaryKey dbHead
  where
    firstText = Api.serialiseAddress first
    secondText = Api.serialiseAddress second

updateHydraHeadStatusQ :: MonadBeam Postgres m => Int32 -> HydraHeadStatus -> m ()
updateHydraHeadStatusQ headId status = do
  mHead <- runSelectReturningOne $ select $ do
    head_ <- all_ $ Db.db ^. Db.db_heads

    guard_ (head_ ^. Db.hydraHead_id ==. val_ (SqlSerial headId))
    pure head_

  case mHead of
    Just head_ -> do
      when (head_ ^. Db.hydraHead_status < status) $ runUpdate $ update (Db.db ^. Db.db_heads)
        (\h_ -> h_ ^. Db.hydraHead_status <-. val_ status)
        (\h_ -> h_ ^. Db.hydraHead_id ==. val_ (SqlSerial headId))
    Nothing -> pure ()

updatePaymentChannelStatusQ :: MonadBeam Postgres m => Int32 -> PaymentChannelStatus -> m ()
updatePaymentChannelStatusQ pcid status = do
  mChan <- runSelectReturningOne $ select $ do
    chan <- all_ $ Db.db ^. Db.db_paymentChannels
    guard_ (chan ^. Db.paymentChannel_id ==. val_ (SqlSerial pcid))
    pure chan

  case mChan of
    Just chan -> do
      when (chan ^. Db.paymentChannel_status < status) $ runUpdate $ update (Db.db ^. Db.db_paymentChannels)
        (\channel -> channel ^. Db.paymentChannel_status <-. val_ status)
        (\channel -> channel ^. Db.paymentChannel_id ==. val_ (SqlSerial pcid)
        )
    Nothing -> pure ()

tryUpdatePaymentChannelForHeadStatusQ :: MonadBeam Postgres m => Int32 -> PaymentChannelStatus -> m ()
tryUpdatePaymentChannelForHeadStatusQ headId status = do
  mChan <- runSelectReturningOne $ select $ do
    chan <- all_ $ Db.db ^. Db.db_paymentChannels
    guard_ (chan ^. Db.paymentChannel_head ==. val_ (Db.HeadId (SqlSerial headId)))
    pure chan

  case mChan of
    Just chan -> do
      when (chan ^. Db.paymentChannel_status < status) $ runUpdate $ update (Db.db ^. Db.db_paymentChannels)
        (\channel -> channel ^. Db.paymentChannel_status <-. val_ status)
        (\channel -> channel ^. Db.paymentChannel_head ==. val_ (Db.HeadId (SqlSerial headId))
        )
    Nothing -> pure ()

getHydraHeadStatusQ :: MonadBeam Postgres m => Int32 -> m HydraHeadStatus
getHydraHeadStatusQ headId = do
  mHead <- runSelectReturningOne $ select $ do
    head_ <- all_ $ Db.db ^. Db.db_heads
    guard_ (head_ ^. Db.hydraHead_id ==. val_ (SqlSerial headId))
    pure head_
  pure $ maybe HydraHeadStatus_Unknown Db._hydraHead_status mHead

checkAddressAvailability :: MonadBeam Postgres m => Api.AddressAny -> m Bool
checkAddressAvailability addr = do
  isAvail <- runSelectReturningOne $ select $ do
    aa <- all_ $ Db.db ^. Db.db_addressAvailability
    guard_ (aa ^. Db.addressAvailability_layer1Address ==. val_ (Api.serialiseAddress addr))
    pure $ aa ^. Db.addressAvailability_isAvailable
  case isAvail of
    Nothing -> return True
    Just avail -> return avail
