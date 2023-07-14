{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Proxy where

import Data.Int
import Data.Foldable

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
import HydraPay.Types
import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Cardano.Hydra.Tools
import qualified HydraPay.Database as Db

data ProxyInfo = ProxyInfo
  { _proxyInfo_address :: ProxyAddress
  , _proxyInfo_internalWalletAddress :: Api.AddressAny
  , _proxyInfo_verificationKey :: FilePath
  , _proxyInfo_signingKey :: FilePath
  , _proxyInfo_hydraVerificationKey :: FilePath
  , _proxyInfo_hydraSigningKey :: FilePath
  , _proxyInfo_internalWalletVerificationKey :: FilePath
  , _proxyInfo_internalWalletSigningKey :: FilePath
  }
  deriving (Eq, Show)

makeLenses ''ProxyInfo

getProxyInfo :: MonadBeam Postgres m => Int32 -> Api.AddressAny -> m (Maybe ProxyInfo)
getProxyInfo hid addr = do
  proxy <- runSelectReturningOne $ select $ do
    h <- all_ (Db.db ^. Db.db_heads)
    proxy <- all_ (Db.db ^. Db.db_proxies)
    ph <- all_ (Db.db ^. Db.db_proxyHead)
    guard_ (h ^. Db.hydraHead_id ==. (val_ $ SqlSerial hid))
    guard_ (proxy ^. Db.proxy_chainAddress ==. val_ (addr ^. to Api.serialiseAddress))
    guard_ ((ph ^. Db.proxyHead_head) `references_` h)
    guard_ ((ph ^. Db.proxyHead_proxy) `references_` proxy)
    pure proxy
  pure $ proxy >>= dbProxyInfoToProxyInfo

getProxyInfoDirect :: MonadBeam Postgres m => Int32 -> m (Maybe ProxyInfo)
getProxyInfoDirect pid = do
  proxy <- runSelectReturningOne $ select $ do
    proxy <- all_ (Db.db ^. Db.db_proxies)
    guard_ (proxy ^. Db.proxy_id ==. val_ (SqlSerial pid))
    pure proxy
  pure $ proxy >>= dbProxyInfoToProxyInfo

-- | Return the chain address given a proxy address.
getProxyChainAddressAndSigningKey :: MonadBeam Postgres m => Api.AddressAny -> m (Maybe (Api.AddressAny, Text))
getProxyChainAddressAndSigningKey addr = do
  chainAddress <- runSelectReturningOne $ select $ do
    proxy <- all_ (Db.db ^. Db.db_proxies)
    guard_ (proxy ^. Db.proxy_hydraAddress ==.  val_ (Api.serialiseAddress addr))
    pure (Db._proxy_chainAddress proxy, Db._proxy_signingKeyPath proxy)
  pure $ do
    (c, sk) <- chainAddress
    a <- Api.deserialiseAddress Api.AsAddressAny c
    pure (a, sk)

addProxyInfo :: MonadBeamInsertReturning Postgres m => Int32 -> Api.AddressAny -> ProxyInfo -> m (Maybe ProxyInfo)
addProxyInfo hid addr pinfo = do
  result <- runInsertReturningList
    -- If somehow we had someone insert before we did, we should get the information back out and use that
    $ insertOnConflict (Db.db ^. Db.db_proxies)
      (insertExpressions [ Db.ProxyInfo
                           default_
                           (val_ $ Api.serialiseAddress addr)
                           (val_ $ pinfo ^. proxyInfo_address . to (Api.serialiseAddress . unProxyAddress))
                           (val_ $ pinfo ^. proxyInfo_internalWalletAddress . to (Api.serialiseAddress))
                           (val_ $ pinfo ^. proxyInfo_verificationKey . to (T.pack))
                           (val_ $ pinfo ^. proxyInfo_signingKey . to (T.pack))
                           (val_ $ pinfo ^. proxyInfo_hydraVerificationKey . to (T.pack))
                           (val_ $ pinfo ^. proxyInfo_hydraSigningKey . to (T.pack))
                           (val_ $ pinfo ^. proxyInfo_internalWalletVerificationKey . to (T.pack))
                           (val_ $ pinfo ^. proxyInfo_internalWalletSigningKey . to (T.pack))
                    ])
      anyConflict onConflictDoNothing

  -- Create the linkages for the new proxy info
  for_ result $ \pi_ -> do
    runInsertReturningList $ insert (Db.db ^. Db.db_proxyHead) $ insertExpressions
      [ Db.ProxyHead default_ (val_ $ pk pi_) (val_ $ Db.HeadId $ SqlSerial hid)
      ]

  pure $ headMaybe result >>= dbProxyInfoToProxyInfo

queryProxyInfo :: (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, HasNodeInfo a, MonadIO m) => a -> Int32 -> Api.AddressAny -> m (Either Text ProxyInfo)
queryProxyInfo a headId addr = do
  result <- getProxyInfo headId addr
  case result of
    Nothing -> do
      runExceptT $ do
        let
          prefix = show headId <> "-" <> (T.unpack $ T.takeEnd 8 $ Api.serialiseAddress addr)
          proxyKeysPath = "proxy-keys"
        (vk, sk) <- ExceptT $ runCardanoCli a $ keyGen $ keyGenTemplate proxyKeysPath $ prefix <> ".cardano"
        (ivk, isk) <- ExceptT $ runCardanoCli a $ keyGen $ keyGenTemplate proxyKeysPath $ prefix <> ".internal"
        proxyAddr <- ExceptT $ runCardanoCli a $ buildAddress vk
        internalWalletAddress <- ExceptT $ runCardanoCli a $ buildAddress ivk
        (hvk, hsk) <- ExceptT $ hydraKeyGen $ proxyKeysPath </> (prefix <> ".hydra")
        let newInfo = ProxyInfo (ProxyAddress proxyAddr) internalWalletAddress vk sk hvk hsk ivk isk
        ExceptT $ fmap (maybeToEither "Failed to read Proxy Info from database") $ addProxyInfo headId addr newInfo
    Just info -> do
      pure $ Right info

dbProxyInfoToProxyInfo :: Db.ProxyInfo -> Maybe ProxyInfo
dbProxyInfoToProxyInfo pinfo =
  ProxyInfo
  <$> (fmap ProxyAddress $ Api.deserialiseAddress Api.AsAddressAny $ pinfo ^. Db.proxy_hydraAddress)
  <*> (Api.deserialiseAddress Api.AsAddressAny $ pinfo ^. Db.proxy_internalWalletAddress)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_verificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_signingKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_hydraVerificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_hydraSigningKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_internalWalletVerificationKeyPath)
  <*> (pure $ T.unpack $ pinfo ^. Db.proxy_internalWalletSigningKeyPath)
