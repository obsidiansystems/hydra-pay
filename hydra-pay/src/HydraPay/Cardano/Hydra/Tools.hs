{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Hydra.Tools where

import System.Which
import System.Process
import System.Directory
import System.FilePath

import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.Text (Text)
import qualified Data.Text as T
import Data.Bifunctor

import HydraPay.Utils

hydraToolsPath :: FilePath
hydraToolsPath = $(staticWhich "hydra-tools")

-- | Generates a pair of hydra keys given a incomplete path like path/to/patternname, it will create path/to/patternname.{vk|sk}
-- the interface mimics the underlying hydra-tools command line, and thus is limited to path/to/patternname.{vk|sk}
hydraKeyGen :: MonadIO m => String -> m (Either Text (FilePath, FilePath))
hydraKeyGen filePattern = fmap (first T.pack) $ runExceptT $ do
  liftIO $ createDirectoryIfMissing True $ takeDirectory filePattern
  _ <- ExceptT $ eitherReadProcess $ proc "hydra-tools" ["gen-hydra-key", "--output-file", filePattern]
  pure (filePattern <> ".vk", filePattern <> ".sk")
