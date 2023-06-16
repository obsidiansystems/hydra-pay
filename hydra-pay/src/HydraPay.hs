{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay where

import System.Which
import System.Directory
import System.IO
import System.IO.Temp

import Control.Exception (catch)
import Data.Aeson.Lens
import Data.Bifunctor

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra
import HydraPay.Database.Workers (RefundRequest(..))
import HydraPay.Proxy
import HydraPay.Transaction
import qualified HydraPay.Database as DB
import HydraPay.PaymentChannel.Postgres (getExpiredPaymentChannels)
import HydraPay.PaymentChannel

import Control.Concurrent (threadDelay)

import qualified Data.Aeson as Aeson
import Data.Int
import qualified Data.ByteString.Char8 as B8
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Cardano.Transaction hiding (TxId)
import Cardano.Transaction.CardanoApi
import Cardano.Transaction.Extras (evalRawNoSubmit)

import Data.Pool
import Gargoyle.PostgreSQL.Connect
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Query

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

data HydraPayState = HydraPayState
  { _hydraPay_nodeInfo :: NodeInfo
  , _hydraPay_databaseConnectionPool :: Pool Connection
  , _hydraPay_logger :: Logger
  , _hydraPay_hydraHeadManager :: HydraHeadManager
  }

makeLenses ''HydraPayState

instance HasNodeInfo HydraPayState where
  nodeInfo = hydraPay_nodeInfo

instance DB.HasDbConnectionPool HydraPayState where
  dbConnectionPool = hydraPay_databaseConnectionPool

class HasHydraPay a where
  hydraPay :: Lens' a HydraPayState

instance HasHydraPay HydraPayState where
  hydraPay = id

instance HasLogger HydraPayState where
  getLogger = hydraPay_logger

instance HasHydraHeadManager HydraPayState where
  hydraHeadManager = hydraPay_hydraHeadManager

data HydraPayConfig = HydraPayConfig
  { hydraPaySettings_database :: FilePath
  , hydraPaySettings_logSettings :: LogConfig
  , hydraPaySettings_nodeConfig :: NodeConfig
  }

runHydraPay :: HydraPayConfig -> (HydraPayState -> IO a) -> IO a
runHydraPay (HydraPayConfig db ls ncfg) action = withLogger ls $ \l -> withDb db $ \pool -> do
  withResource pool DB.doAutomigrate
  withCardanoNode ncfg $ \ni -> do
    withHydraHeadManager $ \manager -> do
      action $ HydraPayState ni pool l manager

sendFuelTo :: (MonadIO m, HasNodeInfo a) => a -> Api.ProtocolParameters -> Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> m TxId
sendFuelTo a pparams fromAddr skPath toAddr amount = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a paramsPath
    fmap (TxId . T.pack) $
      eval cfg (payFuelTo fromAddr skPath toAddr amount) `catch` \e@(EvalException _ _ _) -> print e >> pure "FAKE TX ID"

getProxyTx :: (HasNodeInfo a, DB.HasDbConnectionPool a, MonadIO m) => a -> Api.ProtocolParameters -> Int32 -> Api.AddressAny -> Int32 -> m (Either Text BS.ByteString)
getProxyTx a pparams hid addr lovelace = DB.runBeam a $ runExceptT $ do
  proxyInfo <- ExceptT $ queryProxyInfo a hid addr
  ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a paramsPath
    txLbs <- fmap LBS.pack $ evalTx cfg $ payToProxyTx addr lovelace proxyInfo
    pure $ fmap (BS.pack . T.unpack) $ maybeToEither "Failed to decode cborhex" $ txLbs ^? key "cborHex" . _String

assumeWitnessed :: BS.ByteString -> BS.ByteString
assumeWitnessed bs = do
  BS.unlines [ "{"
             , "\"type\": \"Tx BabbageEra\","
             , "\"description\": \"Ledger Cddl Format\","
             , "\"cborHex\": \"" <> bs <> "\""
             , "}"
             ]

submitTxCbor :: (HasLogger a, HasNodeInfo a, MonadIO m) => a -> BS.ByteString -> m (Either Text TxId)
submitTxCbor state cbor = runExceptT $ do
  liftIO $ createDirectoryIfMissing True tempTxDir
  ExceptT $ liftIO $ withTempFile tempTxDir "tx" $ \fp handle -> do
    logInfo state "submitTxCbor" $ "This is the CBOR: " <> tShow cbor
    BS.hPutStr handle $ assumeWitnessed cbor
    hClose handle
    logInfo state "submitTxCbor" "Submitting signed transaction"
    _ <- runCardanoCli state $ submitTxFile fp
    runCardanoCli state $ getTxId fp

waitForTxInput :: (HasLogger a, HasHydraPay a, HasNodeInfo a, MonadIO m) => a -> TxInput -> m (Either Text ())
waitForTxInput state txin = runExceptT $ do
  logInfo state "waitForTxInput" $ "Waiting for transaction " <> txInputToText txin
  exists <- ExceptT $ runCardanoCli state $ txInExists txin
  case exists of
    True -> pure ()
    False -> do
      liftIO $ threadDelay 500000
      ExceptT $ waitForTxInput state txin

transferTo :: Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> Maybe BS.ByteString -> Tx ()
transferTo fromAddr skPath toAddr lovelace mDatum = do
  Output {..} <- case mDatum of
    Just dh -> outputWithDatumHash toStr (fromString $ show lovelace <> " lovelace") (BS.unpack dh)
    Nothing -> output toStr $ fromString $ show lovelace <> " lovelace"
  void $ selectInputs oValue fromStr
  changeAddress fromStr
  void $ balanceNonAdaAssets fromStr
  sign skPath
  where
    toStr = addressString toAddr
    fromStr = addressString fromAddr

payFuelTo :: Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> Tx ()
payFuelTo fromAddr skPath toAddr lovelace =
  transferTo fromAddr skPath toAddr lovelace $ Just fuelMarkerDatumHash

payToProxyTx :: Api.AddressAny -> Int32 -> ProxyInfo -> Tx ()
payToProxyTx addr lovelace proxy = do
  Output {..} <- output (proxy ^. proxyInfo_address . to addressString) $ fromString $ show lovelace <> " lovelace"
  void $ selectInputs oValue addrStr
  changeAddress addrStr
  void $ balanceNonAdaAssets addrStr
  where
    addrStr = addressString addr

-- | Like 'Cardano.Transaction.selectInputs', but allows to provide your own
-- UTxO. In the case of Hydra the UTxO comes from talking to the Head which
-- 'Cardano.Transaction.selectInputs' can not handle.
selectInputsFromUTxO :: Value -> Api.UTxO Api.BabbageEra -> Tx ([Input], Value)
selectInputsFromUTxO outputValue utxo = do
  let inputs = inputFromUTxO <$> fromCardanoApiUTxO utxo
  putpend $ mempty { tInputs = inputs }
  -- Merge the utxos values
  let mergeInputValue = mconcat $ map (utxoValue . iUtxo) inputs
  -- return the inputs and the remaining outputs
  pure (inputs, diffValuesWithNegatives outputValue mergeInputValue)

sendHydraLovelaceTx :: Api.UTxO Api.BabbageEra -> Address -> Address -> Api.Lovelace -> Tx ()
sendHydraLovelaceTx utxo fromAddrStr toAddrStr lovelace = do
  Output {..} <- output toAddrStr $ fromString $ show (Api.lovelaceToQuantity lovelace) <> " lovelace"
  void $ selectInputsFromUTxO oValue utxo
  void $ balanceAdaAssets fromAddrStr

balanceAdaAssets
  :: Address
  -- ^ Change address
  -> Tx (Maybe Output)
balanceAdaAssets addr = do
  TransactionBuilder {..} <- getTransactionBuilder
  let
    inputValue = mconcat $ map (utxoValue . iUtxo) tInputs
    outputValue = mconcat $ map oValue tOutputs
    theDiffValue = inputValue `diffValues` outputValue

    -- Make sure there are non-ada assets in there

  if theDiffValue == mempty then pure Nothing else do
    Just <$> output addr theDiffValue

sendHydraLovelace :: (MonadIO m, HasNodeInfo a) => a -> ProxyInfo -> Api.ProtocolParameters -> Api.UTxO Api.BabbageEra -> Address -> Address -> Api.Lovelace -> m (Either Text (Text, Text))
sendHydraLovelace a proxyInfo pparams utxo fromAddrStr toAddrStr lovelace = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a paramsPath
        fee = 0
    (txId, txLbs) <- evalRawNoSubmit cfg fee $ do
      sendHydraLovelaceTx utxo fromAddrStr toAddrStr lovelace
      sign (_proxyInfo_signingKey proxyInfo)
    case txLbs ^? key "cborHex" . _String of
      Nothing ->
        pure $ Left "sendHydraLovelace: could not decode cardano tx creation"
      Just cbor -> do
        pure $ Right (T.pack txId, cbor)

-- Handles refunds for payment channel initiators whose invites have expired
handleChannelRefund :: MonadIO m => HydraPayState -> RefundRequest -> m (Either Text Text)
handleChannelRefund state refundRequest = do
  eHydraAddr <- eitherAddrDeserialise $ _refundRequest_hydraAddress refundRequest
  case eHydraAddr of
    Left err -> return $ Left err
    Right hydraAddr -> do
      eChainAddr <- eitherAddrDeserialise $ _refundRequest_chainAddress refundRequest
      case eChainAddr of
        Left err -> return $ Left err
        Right chainAddr -> do
          handle <- liftIO $ openFile (T.unpack $ _refundRequest_protocolParams refundRequest) ReadMode
          pparamBytes <- liftIO $ hGetContents handle
          ePparams <- fmap (first T.pack) $ runExceptT $
             ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack pparamBytes
          case ePparams of
            Left err -> return $ Left err
            Right pparams -> do
              txid <- fanoutToL1Address state pparams hydraAddr (_refundRequest_signingKeyPath refundRequest) chainAddr $ _refundRequest_amount refundRequest
              eRes <- runExceptT $ ExceptT $ waitForTxInput state $ mkTxInput txid 0
              case eRes of
                Left txErr -> return $ Left txErr
                Right _ -> do
                  -- Mark payment channel as closed
                  DB.runBeam (_hydraPay_databaseConnectionPool state) $ runUpdate $
                    update
                    (DB.db ^. DB.db_paymentChannels)
                    (\channel -> channel ^. DB.paymentChannel_status <-. val_ PaymentChannelStatus_Closed)
                    (\channel -> channel ^. DB.paymentChannel_head ==. val_ (DB.HeadId (SqlSerial (_refundRequest_hydraHead refundRequest))))
                  return $ Right $ unTxId txid
  where
    mayToEitherAddr = maybeToEither "Failed to deserialize to Any Address"
    eitherAddrDeserialise txt = fmap (first T.pack) $ runExceptT $
      ExceptT $ pure $ mayToEitherAddr $ Api.deserialiseAddress Api.AsAddressAny $ txt
