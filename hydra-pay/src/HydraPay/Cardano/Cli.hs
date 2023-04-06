{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Cli where

import HydraPay.Cardano.Node

import System.Exit
import System.Which
import System.Process

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as Aeson

import qualified Data.Text as T

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS

import qualified Cardano.Api as Api

data CardanoCliCommand :: * -> * where
  CardanoCliCommand_QueryTip :: CardanoCliCommand (Either String Aeson.Value)
  CardanoCliCommand_GetProtocolParams :: CardanoCliCommand (Either String Aeson.Value)
  CardanoCliCommand_QueryUTXOs :: Api.AddressAny -> CardanoCliCommand (Either String (Api.UTxO Api.BabbageEra))

runCardanoCli :: MonadIO m => NodeInfo -> CardanoCliCommand a -> m a
runCardanoCli cfg command = case command of
  CardanoCliCommand_QueryTip -> do
    runExceptT $ do
      str <- ExceptT $ eitherReadProcess cp
      ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

  CardanoCliCommand_GetProtocolParams -> do
    runExceptT $ do
      str <- ExceptT $ eitherReadProcess cp
      ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

  CardanoCliCommand_QueryUTXOs _ -> do
    runExceptT $ do
      str <- ExceptT $ eitherReadProcess cp
      ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

  where
    cp = makeCliProcess cfg command

queryTip :: CardanoCliCommand (Either String Aeson.Value)
queryTip = CardanoCliCommand_QueryTip

getProtocolParameters :: CardanoCliCommand (Either String Aeson.Value)
getProtocolParameters = CardanoCliCommand_GetProtocolParams

queryUTxOs :: Api.AddressAny -> CardanoCliCommand (Either String (Api.UTxO Api.BabbageEra))
queryUTxOs = CardanoCliCommand_QueryUTXOs

eitherReadProcess :: MonadIO m => CreateProcess -> m (Either String String)
eitherReadProcess cp = do
  (code, out, err) <- liftIO $ readCreateProcessWithExitCode cp ""
  case code of
    ExitSuccess -> pure $ Right out
    ExitFailure _ -> pure $ Left err

makeCliProcess :: NodeInfo -> CardanoCliCommand a -> CreateProcess
makeCliProcess nodeInfo command =
  addSocketPath process
  where
    process = case command of
      CardanoCliCommand_QueryTip ->
        base ["query", "tip", "--testnet-magic", magic]
      CardanoCliCommand_GetProtocolParams ->
        base  ["query", "protocol-parameters", "--testnet-magic", magic]
      CardanoCliCommand_QueryUTXOs addr ->
        base ["query", "utxos", T.unpack $ Api.serialiseAddress addr]

    base = proc cardanoCliPath

    addSocketPath p =
      p { env = Just [("CARDANO_NODE_SOCKET_PATH", nodeInfo ^. nodeInfo_socket)]
        }

    magic = show $ nodeInfo ^. nodeInfo_magic

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")
