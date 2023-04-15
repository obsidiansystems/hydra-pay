{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HydraPay where

import System.Which
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

import HydraPay.Logging
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import qualified HydraPay.Database as DB

import Control.Concurrent.STM

import Data.Int
import Data.String
import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import ByteString.Aeson.Orphans

import Data.Map (Map)
import qualified Data.Map as Map

import Cardano.Transaction

import Data.Pool
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Gargoyle.PostgreSQL.Connect
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions

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
  , _hydraPay_databaseConnectionPool :: Pool Connection
  , _hydraPay_logger :: Logger
  }

makeLenses ''HydraPayState

instance HasNodeInfo HydraPayState where
  nodeInfo = hydraPay_nodeInfo

class HasHydraPay a where
  hydraPay :: Lens' a HydraPayState

instance HasHydraPay HydraPayState where
  hydraPay = id

instance HasLogger HydraPayState where
  getLogger = hydraPay_logger

data HydraPayConfig = HydraPayConfig
  { hydraPaySettings_database :: FilePath
  , hydraPaySettings_logSettings :: LogConfig
  , hydraPaySettings_nodeConfig :: NodeConfig
  }

runHydraPay :: HydraPayConfig -> (HydraPayState -> IO a) -> IO a
runHydraPay (HydraPayConfig db ls ncfg) action = withLogger ls $ \l -> withDb db $ \pool -> do
  withResource pool DB.doAutomigrate
  withCardanoNode ncfg $ \ni -> do
    proxies <- newTMVarIO mempty
    action $ HydraPayState proxies ni pool l

withTMVar :: MonadIO m => TMVar a -> (a -> m (a, b)) -> m b
withTMVar var action = do
  val <- liftIO $ atomically $ takeTMVar var
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
  where
    addrStr = addressString addr

queryProxyInfo :: (HasNodeInfo a, HasHydraPay a, MonadIO m) => a -> Api.AddressAny -> m (Either String ProxyInfo)
queryProxyInfo a addr = do
  result <- runQueryInTransaction a $ getProxyInfo addr
  case result of
    Nothing -> do
      runExceptT $ do
        (vk, sk) <- ExceptT $ runCardanoCli a $ keyGen $ KeyGenConfig "proxy-keys" $ T.unpack $ T.takeEnd 8 $ Api.serialiseAddress addr
        proxyAddr <- ExceptT $ runCardanoCli a $ buildAddress vk
        let newInfo = ProxyInfo proxyAddr vk sk
        ExceptT $ fmap (maybeToEither "Failed to read Proxy Info from database") $ runQueryInTransaction a $ addProxyInfo addr newInfo
    Just info -> do
      pure $ Right info

runQueryInTransaction :: (HasHydraPay a, MonadIO m) => a -> (Connection -> IO b) -> m b
runQueryInTransaction a action = liftIO $ withResource pool $ \conn -> do
  withTransaction conn (action conn)
  where
    pool = a ^. hydraPay . hydraPay_databaseConnectionPool

getProxyInfo :: MonadIO m => Api.AddressAny -> Connection -> m (Maybe ProxyInfo)
getProxyInfo addr conn = liftIO $ do
  proxy <- runBeamPostgres conn $ runSelectReturningOne $ select $ do
    proxy <- all_ (DB.db ^. DB.db_proxies)
    guard_ (proxy ^. DB.proxy_chainAddress ==.  val_ (Api.serialiseAddress addr))
    pure proxy
  pure $ proxy >>= dbProxyInfoToProxyInfo

addProxyInfo :: MonadIO m => Api.AddressAny -> ProxyInfo -> Connection -> m (Maybe ProxyInfo)
addProxyInfo addr pinfo conn = liftIO $ do
  result <- runBeamPostgres conn
    $ runInsertReturningList
    $ insertOnConflict (DB.db ^. DB.db_proxies)
    (insertValues [ DB.ProxyInfo
                    (Api.serialiseAddress addr)
                    (pinfo ^. proxyInfo_address . to (Api.serialiseAddress))
                    (pinfo ^. proxyInfo_verificationKey . to (T.pack))
                    (pinfo ^. proxyInfo_signingKey . to (T.pack))
                  ])
    -- If somehow we had someone insert before we did, we should get the information back out and use that
    anyConflict onConflictDoNothing
  pure $ headMaybe result >>= dbProxyInfoToProxyInfo

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

dbProxyInfoToProxyInfo :: DB.ProxyInfo -> Maybe ProxyInfo
dbProxyInfoToProxyInfo pinfo =
  ProxyInfo
  <$> (Api.deserialiseAddress Api.AsAddressAny $ pinfo ^. DB.proxy_chainAddress)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_verificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_signingKeyPath)
