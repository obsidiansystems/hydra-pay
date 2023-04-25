{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HydraPay where

import System.Which
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp

import Data.Aeson.Lens

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Logging
import HydraPay.PaymentChannel
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra
import HydraPay.Cardano.Hydra.Tools
import HydraPay.Proxy
import qualified HydraPay.Database as DB

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Data.Int
import Data.String
import Data.Text (Text)
import Data.Bifunctor
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import ByteString.Aeson.Orphans

import Data.Map (Map)
import qualified Data.Map as Map

import Cardano.Ledger.SafeHash as Hash
import Cardano.Crypto.Hash.Class as Hash
import Cardano.Transaction hiding (TxId)

import Data.Pool
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Gargoyle.PostgreSQL.Connect
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

data HydraPayState = HydraPayState
  { _hydraPay_proxies :: TMVar (Map Api.AddressAny ProxyInfo)
  , _hydraPay_nodeInfo :: NodeInfo
  , _hydraPay_databaseConnectionPool :: Pool Connection
  , _hydraPay_logger :: Logger
  , _hydraPay_paymentChannelManager :: PaymentChannelManager
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

instance HasPaymentChannelManager HydraPayState where
  paymentChannelManager = hydraPay_paymentChannelManager

data HydraPayConfig = HydraPayConfig
  { hydraPaySettings_database :: FilePath
  , hydraPaySettings_logSettings :: LogConfig
  , hydraPaySettings_nodeConfig :: NodeConfig
  }

runHydraPay :: HydraPayConfig -> (HydraPayState -> IO a) -> IO a
runHydraPay (HydraPayConfig db ls ncfg) action = withLogger ls $ \l -> withDb db $ \pool -> do
  withResource pool DB.doAutomigrate
  withCardanoNode ncfg $ \ni -> do
    withPaymentChannelManager $ \manager -> do
      proxies <- newTMVarIO mempty
      action $ HydraPayState proxies ni pool l manager

addressString :: Api.AddressAny -> String
addressString = T.unpack . Api.serialiseAddress

-- | Given node information, and a path to the node socket and protocol parameters create an EvalConfig for running the Tx monad
mkEvalConfig :: HasNodeInfo a => a -> FilePath -> FilePath -> EvalConfig
mkEvalConfig a nsFp ppFp = EvalConfig Nothing (ni ^. nodeInfo_magic . to (Just . fromIntegral)) (Just ppFp) False (Just nsFp)
  where
    ni = a ^. nodeInfo

sendFuelTo :: (MonadIO m, HasNodeInfo a) => a -> Api.ProtocolParameters -> Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> m TxId
sendFuelTo a pparams from skPath to amount = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a socketPath paramsPath
    fmap (TxId . T.pack) $ eval cfg $ payFuelTo from skPath to amount
  where
    socketPath = a ^. nodeInfo . nodeInfo_socketPath

-- | cardano-cli needs the params in a file, so we just create a temp file we can use for that purpose
withProtocolParamsFile :: Api.ProtocolParameters -> (FilePath -> IO a) -> IO a
withProtocolParamsFile pparams action = do
  createDirectoryIfMissing True tempTxDir
  withTempFile tempTxDir "params" $ \paramsPath handle -> do
    hClose handle
    Aeson.encodeFile paramsPath pparams
    action paramsPath

getProxyTx :: (HasHydraPay a, HasNodeInfo a, DB.HasDbConnectionPool a, MonadIO m) => a -> Api.ProtocolParameters -> Api.AddressAny -> Int32 -> m (Either Text BS.ByteString)
getProxyTx a pparams addr lovelace = runExceptT $ do
  proxyInfo <- ExceptT $ queryProxyInfo a addr
  ExceptT $ liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a socketPath paramsPath
    txLbs <- fmap LBS.pack $ evalTx cfg $ payToProxyTx addr lovelace proxyInfo
    -- NOTE(skylar): For some reason the _JSON key doesn't work in this case, maybe it has to do with the instance for ByteString
    pure $ fmap (BS.pack . T.unpack) $ maybeToEither "Failed to decode cborhex" $ txLbs ^? key "cborHex" . _String
  where
    socketPath = a ^. nodeInfo . nodeInfo_socketPath

tempTxDir :: FilePath
tempTxDir = "tx"

assumeWitnessed :: BS.ByteString -> BS.ByteString
assumeWitnessed bs = do
  BS.unlines [ "{"
             , "\"type\": \"Tx BabbageEra\","
             , "\"description\": \"Ledger Cddl Format\","
             , "\"cborHex\": \"" <> bs <> "\""
             , "}"
             ]

submitTxCbor :: (HasLogger a, HasHydraPay a, HasNodeInfo a, MonadIO m) => a -> BS.ByteString -> m (Either Text TxId)
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
      liftIO $ threadDelay 100000
      ExceptT $ waitForTxInput state txin

payFuelTo :: Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> Tx ()
payFuelTo from skPath to lovelace = do
  Output {..} <- outputWithDatumHash (addressString to) (fromString $ show lovelace <> " lovelace") (BS.unpack fuelMarkerDatumHash)
  void $ selectInputs oValue fromStr
  changeAddress fromStr
  void $ balanceNonAdaAssets fromStr
  sign skPath
  where
    fromStr = addressString from

payToProxyTx :: Api.AddressAny -> Int32 -> ProxyInfo -> Tx ()
payToProxyTx addr lovelace proxy = do
  Output {..} <- output (proxy ^. proxyInfo_address . to addressString) $ fromString $ show lovelace <> " lovelace"
  void $ selectInputs oValue addrStr
  changeAddress addrStr
  void $ balanceNonAdaAssets addrStr
  where
    addrStr = addressString addr

totalLovelace :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalLovelace = sum . fmap (txOutValue) . filter (not . txOutIsFuel) . Map.elems . Api.unUTxO

totalLovelace' :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalLovelace' = sum . fmap (txOutValue) . Map.elems . Api.unUTxO

totalFuelLovelace :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalFuelLovelace = sum . fmap (txOutValue) . filter txOutIsFuel . Map.elems . Api.unUTxO

fuelMarkerDatumHash :: BS.ByteString
fuelMarkerDatumHash =
  "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"

txOutDatumIsFuel :: Api.TxOutDatum ctx era -> Bool
txOutDatumIsFuel (Api.TxOutDatumHash _ datumHash) =
  Aeson.encode datumHash == LBS.fromStrict ("\"" <> fuelMarkerDatumHash <> "\"")
txOutDatumIsFuel _ = False

txOutIsFuel :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> Bool
txOutIsFuel (Api.TxOut _ _ datum _) = txOutDatumIsFuel datum

txOutValue :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> Api.Lovelace
txOutValue (Api.TxOut _ (Api.TxOutValue _ value) _ _) = Api.selectLovelace value
