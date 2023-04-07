{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HydraPay where

import System.Which
import System.IO
import System.IO.Temp

import Data.Aeson.Lens

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Logger.Extras
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node

import Control.Concurrent.STM

import Data.String

import Data.Int
import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import ByteString.Aeson.Orphans

import Data.Map (Map)
import qualified Data.Map as Map

import Cardano.Transaction

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

data ProxyInfo = ProxyInfo
  { _proxyInfo_address :: Api.AddressAny
  , _proxyInfo_verificationKey :: FilePath
  , _proxyInfo_signingKey :: FilePath
  }
  deriving (Eq, Show)

makeLenses ''ProxyInfo

data HydraPayState = HydraPayState
  { _hydraPay_proxies :: TMVar (Map Api.AddressAny ProxyInfo)
  , _hydraPay_nodeInfo :: NodeInfo
  }

makeLenses ''HydraPayState

instance HasNodeInfo HydraPayState where
  nodeInfo = hydraPay_nodeInfo

class HasHydraPay a where
  hydraPay :: Lens' a HydraPayState

instance HasHydraPay HydraPayState where
  hydraPay = id

runHydraPay :: NodeConfig -> (HydraPayState -> IO a) -> IO a
runHydraPay ncfg action = withCardanoNode ncfg $ \ni -> do
  proxies <- newTMVarIO mempty
  action $ HydraPayState proxies ni

withTMVar :: MonadIO m => TMVar a -> (a -> m (a, b)) -> m b
withTMVar var action = do
  val <- liftIO $ atomically $ readTMVar var
  (a, b) <- action val
  liftIO $ atomically $ putTMVar var a
  pure b

addressString :: Api.AddressAny -> String
addressString = T.unpack . Api.serialiseAddress

-- | Given node information, and a path to the protocol parameters create an EvalConfig for running the Tx monad
mkEvalConfig :: HasNodeInfo a => a -> FilePath -> EvalConfig
mkEvalConfig a ppFp = EvalConfig Nothing (ni ^. nodeInfo_magic . to (Just . fromIntegral)) (Just ppFp) False
  where
    ni = a ^. nodeInfo

getProxyTx :: (HasHydraPay a, HasNodeInfo a, MonadIO m) => a -> Api.ProtocolParameters -> Api.AddressAny -> Int32 -> m (Either String BS.ByteString)
getProxyTx a pparams addr lovelace = runExceptT $ do
  proxyInfo <- ExceptT $ queryProxyInfo a addr
  ExceptT $ liftIO $ withTempFile "proxy-tx" "params" $ \paramsPath handle -> do
    hClose handle
    Aeson.encodeFile paramsPath pparams
    let cfg = mkEvalConfig a paramsPath
    txLbs <- fmap LBS.pack $ evalTx cfg $ payToProxyTx addr lovelace proxyInfo
    pure $ fmap LBS.toStrict $ maybeToEither "Failed to decode cborhex" $ txLbs ^? key "cborHex" . _JSON

payToProxyTx :: Api.AddressAny -> Int32 -> ProxyInfo -> Tx ()
payToProxyTx addr lovelace proxy = do
  Output {..} <- output (proxy ^. proxyInfo_address . to addressString) $ fromString $ show lovelace <> " lovelace"
  void $ selectInputs oValue addrStr
  changeAddress addrStr
  void $ balanceNonAdaAssets addrStr
  pure ()
  where
    addrStr = addressString addr

queryProxyInfo :: (HasHydraPay a, MonadIO m) => a -> Api.AddressAny -> m (Either String ProxyInfo)
queryProxyInfo a addr = withTMVar proxyVar $ \proxies -> do
  case Map.lookup addr proxies of
    Nothing -> do
      result <- runExceptT $ do
        (vk, sk) <- ExceptT $ runCardanoCli hps $ keyGen $ KeyGenConfig "proxy" "test"
        proxyAddr <- ExceptT $ runCardanoCli hps $ buildAddress vk
        pure $ ProxyInfo proxyAddr vk sk
      case result of
        Right newProxy ->
          pure (Map.insert addr newProxy proxies, Right newProxy)
        Left err ->
          pure (proxies, Left err)
    Just a -> do
      pure (proxies, Right a)
  where
    hps = a ^. hydraPay
    proxyVar = hps ^. hydraPay_proxies
