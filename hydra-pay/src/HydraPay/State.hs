{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay.State where

import Data.Int
import Control.Lens
import Data.Bifunctor (first)
import HydraPay.PaymentChannel.Postgres (checkAddressAvailability)
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Query
import HydraPay.Cardano.Hydra.Status
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import HydraPay.Database.Workers (RefundRequest(..))
import Control.Concurrent
import Control.Monad.Trans.Except
import System.Which
import System.Directory
import Control.Monad
import Data.String (fromString)
import System.IO
import System.IO.Temp
import HydraPay.Utils
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import Data.Aeson.Lens
import HydraPay.Transaction
import HydraPay.Cardano.Hydra
import Data.Pool (Pool, withResource)
import Database.Beam.Postgres
import qualified Database.Beam.Postgres as Pg
import HydraPay.Logging
import HydraPay.Proxy
import HydraPay.PortRange
import HydraPay.Types
import HydraPay.Bank
import Data.Text (Text)
import Cardano.Transaction hiding (TxId)
import Cardano.Transaction.CardanoApi
import Cardano.Transaction.Extras (evalRawNoSubmit)
import Cardano.Transaction.Eval
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified HydraPay.Database as DB

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

data HydraPayState = HydraPayState
  { _hydraPay_nodeInfo :: NodeInfo
  , _hydraPay_databaseConnectionPool :: Pool Connection
  , _hydraPay_logger :: Logger
  , _hydraPay_hydraHeadManager :: HydraHeadManager
  , _hydraPay_httpManager :: HTTP.Manager
  , _hydraPay_portRange :: PortRange
  , _hydraPay_bank :: Bank
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

instance HasPortRange HydraPayState where
  portRange = hydraPay_portRange

waitForTxInput :: (HasLogger a, HasHydraPay a, HasNodeInfo a, MonadIO m) => a -> TxInput -> m (Either Text ())
waitForTxInput state txin = runExceptT $ do
  logInfo state "waitForTxInput" $ "Waiting for transaction " <> txInputToText txin
  exists <- ExceptT $ runCardanoCli state $ txInExists txin
  case exists of
    True -> pure ()
    False -> do
      liftIO $ threadDelay 500000
      ExceptT $ waitForTxInput state txin


sendFuelTo :: (MonadIO m, HasNodeInfo a) => a -> Api.ProtocolParameters -> Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> m (Either Text TxId)
sendFuelTo a pparams fromAddr skPath toAddr amount = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a paramsPath
    (fmap . fmap) (TxId . T.pack) $
      evalEither cfg (payFuelTo fromAddr skPath toAddr amount)

getProxyTx :: (HasNodeInfo a, DB.HasDbConnectionPool a, MonadIO m) => a -> Api.ProtocolParameters -> Int32 -> Api.AddressAny -> Int32 -> m (Either Text BS.ByteString)
getProxyTx a pparams hid addr lovelace = DB.runBeam a $ runExceptT $ do
  proxyInfo <- ExceptT $ queryProxyInfo a hid addr
  ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> runExceptT $ do
    let cfg = mkEvalConfig a paramsPath
    txLbs <- ExceptT $ (fmap . fmap) LBS.pack $ evalTxEither cfg $ payToProxyTx addr lovelace proxyInfo
    ExceptT $ pure $ (fmap) (BS.pack . T.unpack) $ maybeToEither "Failed to decode cborhex" $ txLbs ^? key "cborHex" . _String

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
  Output {..} <- output (proxy ^. proxyInfo_address . to (addressString . unProxyAddress)) $ fromString $ show lovelace <> " lovelace"
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
  void $ changeAddress fromAddrStr
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
              result <- runExceptT $ do
                txid <- ExceptT $ fanoutToL1Address state pparams hydraAddr (_refundRequest_signingKeyPath refundRequest) chainAddr $ _refundRequest_amount refundRequest
                eRes <- ExceptT $ waitForTxInput state $ mkTxInput txid 0
                pure (eRes, txid)
              case result of
                Left txErr -> return $ Left txErr
                Right (_, txid) -> do
                  -- Mark payment channel as closed
                  DB.runBeam (_hydraPay_databaseConnectionPool state) $ runUpdate $
                    update
                    (DB.db ^. DB.db_heads)
                    (\head_ -> head_ ^. DB.hydraHead_status <-. val_ HydraHeadStatus_Closed)
                    (\head_ -> head_ ^. DB.hydraHead_id ==. val_ (SqlSerial (_refundRequest_hydraHead refundRequest)))
                  return $ Right $ unTxId txid
  where
    mayToEitherAddr = maybeToEither "Failed to deserialize to Any Address"
    eitherAddrDeserialise txt = fmap (first T.pack) $ runExceptT $
      ExceptT $ pure $ mayToEitherAddr $ Api.deserialiseAddress Api.AsAddressAny $ txt

handleSubmitTx :: MonadIO m => HydraPayState -> Text -> BS.ByteString -> m (Either Text Text)
handleSubmitTx state l1Address tx = do
  case Api.deserialiseAddress Api.AsAddressAny l1Address of
    Nothing -> return $ Left "handleSubmitTx: Could not deserialise address"
    Just addr -> do
      availability <- DB.runBeam (_hydraPay_databaseConnectionPool state) $ checkAddressAvailability addr
      case availability of
        True -> do
          setAvailability l1Address False
          eTx :: Either Text TxId <- submitTxCbor state tx
          case eTx of
            Left err -> do
              -- When error is encountered, ensure the L1 Address is available again
              setAvailability l1Address True
              return $ Left err
            Right txid -> do
              _ <- waitForTxInput state $ mkTxInput txid 0
              setAvailability l1Address True
              return $ Right $ unTxId txid
        False -> do
          return $ Left "Address resource currently unavailable. Please wait..."
  where
    queryAddressAvailability :: MonadIO m => Text -> m [DB.AddressAvailabilityT Identity]
    queryAddressAvailability addr = DB.runBeam (_hydraPay_databaseConnectionPool state) $ do
      runSelectReturningList $ select $ do
        aa <- all_ (DB.db ^. DB.db_addressAvailability)
        guard_ (aa ^. DB.addressAvailability_layer1Address ==. val_ addr)
        pure aa

    setAvailability :: MonadIO m =>Text -> Bool -> m ()
    setAvailability addr avail = DB.runBeam (_hydraPay_databaseConnectionPool state) $ do
      lookupRes <- queryAddressAvailability l1Address
      case lookupRes of
        [] -> do
          _ <- runInsertReturningList $ insert (DB.db ^. DB.db_addressAvailability) $ insertExpressions
            [ DB.AddressAvailability default_ (val_ $ addr) (val_ $ False)
            ]
          return ()
        _:_ -> do
          _ <- runUpdate $
            update
            (DB.db ^. DB.db_addressAvailability)
            (\aa -> aa ^. DB.addressAvailability_isAvailable <-. val_ avail)
            (\aa -> aa ^. DB.addressAvailability_layer1Address ==. val_ l1Address)
          return ()
