{-# LANGUAGE TemplateHaskell #-}

module HydraPay where

import System.Which

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Logger.Extras
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson

import qualified Cardano.Api as Api

import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node

import Control.Concurrent.STM

import Data.Map (Map)
import qualified Data.Map as Map

import Cardano.Transaction

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

data ProxyAccount = ProxyAccount
  { _proxyAccount_address :: Api.AddressAny
  , _proxyAccount_verificationKey :: FilePath
  , _proxyAccount_signingKey :: FilePath
  }
  deriving (Eq, Show)

makeLenses ''ProxyAccount

data HydraPayState = HydraPayState
  { _hydraPay_proxies :: TMVar (Map Api.AddressAny ProxyAccount)
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

queryProxyAddress :: (HasHydraPay a, MonadIO m) => a -> Api.AddressAny -> m (Either String ProxyAccount)
queryProxyAddress a addr = withTMVar proxyVar $ \proxies -> do
  case Map.lookup addr proxies of
    Nothing -> do
      result <- runExceptT $ do
        (vk, sk) <- ExceptT $ runCardanoCli hps $ keyGen $ KeyGenConfig "proxy" "test"
        proxyAddr <- ExceptT $ runCardanoCli hps $ buildAddress vk
        pure $ ProxyAccount proxyAddr vk sk
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
