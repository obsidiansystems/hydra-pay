-- | Bank is the middleman

{-# LANGUAGE TemplateHaskell #-}
module HydraPay.Bank where

import HydraPay.Path
import qualified Cardano.Api as Api
import Cardano.Api.Extras
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import System.Directory

import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node
import HydraPay.Logging

-- Account we get money from
data Bank = Bank
  { _bank_address :: Api.AddressAny
  , _bank_verificationKey :: FilePath
  , _bank_signingKey :: FilePath
  }

-- | Create or fetch the existing bank address
getBank :: (MonadIO m, HasLogger a, HasNodeInfo a) => a -> m Bank
getBank a = liftIO $ do
  exists <- bankExists
  result <- runExceptT $ case exists of
    True -> do
      logInfo a "getBank" "Getting bank information from config"
      addr <- liftIO (doesFileExist bankAddrPath) >>= \case
        True -> do
          addr <- liftIO $ T.readFile bankAddrPath
          pure $ unsafeToAddressAny addr
        False -> do
          addr <- ExceptT $ runCardanoCli a $ buildAddress bankVkPath
          liftIO $ T.writeFile bankAddrPath $ Api.serialiseAddress addr
          logInfo a "getBank" "Bank address cached for faster startup"
          pure addr
      logInfo a "getBank" $ "Bank Address: " <> Api.serialiseAddress addr
      pure $ Bank addr bankVkPath bankSkPath
    False -> do
      logInfo a "getBank" "Generating new bank address"
      _ <- ExceptT $ runCardanoCli a $ keyGen $ keyGenFiles bankVkPath bankSkPath
      addr <- ExceptT $ runCardanoCli a $ buildAddress bankVkPath
      pure $ Bank addr bankVkPath bankSkPath
  case result of
    Left _ -> error "Failed to create bank credentials"
    Right info -> pure info

bankExists :: MonadIO m => m Bool
bankExists =
  liftIO $ foldM (\b a -> doesFileExist a >>= (\x -> pure $ b && x)) True [bankVkPath, bankSkPath]

makeLenses ''Bank
