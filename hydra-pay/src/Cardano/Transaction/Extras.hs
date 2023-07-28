{-# LANGUAGE RecordWildCards #-}
-- |

module Cardano.Transaction.Extras where

import Cardano.Transaction
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import System.Exit
import System.FilePath ((</>))
import System.IO.Temp
import System.Process.Typed

evalRawNoSubmit :: EvalConfig -> Integer -> Tx () -> IO (String, BSL.ByteString)
evalRawNoSubmit EvalConfig {..} fee (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . setSocketPath ecSocketPath . proc cardanoCliPath $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure o -> do
          liftIO $ print o
          liftIO $ print outStr
          liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m (ChainInfo ecTestnet ecSocketPath)) $ mempty
    bodyFlags <- transactionBuilderToRawFlags tempDir ecProtocolParams ecUseRequiredSigners txBuilder fee

    liftIO $ do
      void $ runCardanoCli bodyFlags
      let
        bodyFile = toSigningBodyFlags tempDir
      -- get the txid
      txId <- fmap init $ runCardanoCli $ ["transaction", "txid"] <> bodyFile

      void . runCardanoCli . transactionBuilderToSignFlags tempDir ecTestnet $ txBuilder

      void $ runCardanoCli bodyFlags
      cbor <- BSL.readFile $ tempDir </> "signed-body.txt"
      pure (txId, cbor)

evalRawNoSign :: EvalConfig -> Integer -> Tx () -> IO (String, BSL.ByteString)
evalRawNoSign EvalConfig {..} fee (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . setSocketPath ecSocketPath . proc cardanoCliPath $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure o -> do
          liftIO $ print o
          liftIO $ print outStr
          liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m (ChainInfo ecTestnet ecSocketPath)) $ mempty
    bodyFlags <- transactionBuilderToRawFlags tempDir ecProtocolParams ecUseRequiredSigners txBuilder fee

    liftIO $ do
      void $ runCardanoCli bodyFlags
      let
        bodyFile = toSigningBodyFlags tempDir
      txId <- fmap init $ runCardanoCli $ ["transaction", "txid"] <> bodyFile
      void $ runCardanoCli bodyFlags
      cbor <- BSL.readFile $ tempDir </> "body.txt"
      pure (txId, cbor)
