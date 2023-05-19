{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Proxy where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Cardano.Api as Api
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath

import HydraPay.Utils
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra.Tools
import qualified HydraPay.Database as DB

data ProxyInfo = ProxyInfo
  { _proxyInfo_address :: Api.AddressAny
  , _proxyInfo_verificationKey :: FilePath
  , _proxyInfo_signingKey :: FilePath
  , _proxyInfo_hydraVerificationKey :: FilePath
  , _proxyInfo_hydraSigningKey :: FilePath
  }
  deriving (Eq, Show)

makeLenses ''ProxyInfo

getProxyInfo :: MonadBeam Postgres m => Api.AddressAny -> m (Maybe ProxyInfo)
getProxyInfo addr = do
  proxy <- runSelectReturningOne $ select $ do
    proxy <- all_ (DB.db ^. DB.db_proxies)
    guard_ (proxy ^. DB.proxy_chainAddress ==.  val_ (Api.serialiseAddress addr))
    pure proxy
  pure $ proxy >>= dbProxyInfoToProxyInfo

-- | Return the chain address given a proxy address.
getProxyChainAddressAndSigningKey :: MonadBeam Postgres m => Api.AddressAny -> m (Maybe (Api.AddressAny, Text))
getProxyChainAddressAndSigningKey addr = do
  chainAddress <- runSelectReturningOne $ select $ do
    proxy <- all_ (DB.db ^. DB.db_proxies)
    guard_ (proxy ^. DB.proxy_hydraAddress ==.  val_ (Api.serialiseAddress addr))
    pure (DB._proxy_chainAddress proxy, DB._proxy_signingKeyPath proxy)
  pure $ do
    (c, sk) <- chainAddress
    a <- Api.deserialiseAddress Api.AsAddressAny c
    pure (a, sk)

addProxyInfo :: MonadBeamInsertReturning Postgres m => Api.AddressAny -> ProxyInfo -> m (Maybe ProxyInfo)
addProxyInfo addr pinfo = do
  result <- runInsertReturningList
    $ insertOnConflict (DB.db ^. DB.db_proxies)
    (insertValues [ DB.ProxyInfo
                    (Api.serialiseAddress addr)
                    (pinfo ^. proxyInfo_address . to (Api.serialiseAddress))
                    (pinfo ^. proxyInfo_verificationKey . to (T.pack))
                    (pinfo ^. proxyInfo_signingKey . to (T.pack))
                    (pinfo ^. proxyInfo_hydraVerificationKey . to (T.pack))
                    (pinfo ^. proxyInfo_hydraSigningKey . to (T.pack))
                  ])
    -- If somehow we had someone insert before we did, we should get the information back out and use that
    anyConflict onConflictDoNothing
  pure $ headMaybe result >>= dbProxyInfoToProxyInfo

queryProxyInfo :: (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, HasNodeInfo a, MonadIO m) => a -> Api.AddressAny -> m (Either Text ProxyInfo)
queryProxyInfo a addr = do
  result <- getProxyInfo addr
  case result of
    Nothing -> do
      runExceptT $ do
        let
          prefix = T.unpack $ T.takeEnd 8 $ Api.serialiseAddress addr
          proxyKeysPath = "proxy-keys"
        (vk, sk) <- ExceptT $ runCardanoCli a $ keyGen $ keyGenTemplate proxyKeysPath $ prefix <> ".cardano"
        proxyAddr <- ExceptT $ runCardanoCli a $ buildAddress vk
        (hvk, hsk) <- ExceptT $ hydraKeyGen $ proxyKeysPath </> (prefix <> ".hydra")
        let newInfo = ProxyInfo proxyAddr vk sk hvk hsk
        ExceptT $ fmap (maybeToEither "Failed to read Proxy Info from database") $ addProxyInfo addr newInfo
    Just info -> do
      pure $ Right info

dbProxyInfoToProxyInfo :: DB.ProxyInfo -> Maybe ProxyInfo
dbProxyInfoToProxyInfo pinfo =
  ProxyInfo
  <$> (Api.deserialiseAddress Api.AsAddressAny $ pinfo ^. DB.proxy_hydraAddress)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_verificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_signingKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_hydraVerificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. DB.proxy_hydraSigningKeyPath)
