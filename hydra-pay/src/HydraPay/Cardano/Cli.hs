{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Cli where

import HydraPay.Cardano.Node

import System.Exit
import System.Which
import System.Process
import System.Directory

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Data.Aeson as Aeson

import qualified Data.Text as T

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

data KeyGenConfig = KeyGenConfig
  { _keyGenConfig_path :: FilePath
  , _keyGenConfig_name :: String
  }
  deriving (Eq, Show)

makeLenses ''KeyGenConfig

keyGenConfigPathPrefix :: KeyGenConfig -> FilePath
keyGenConfigPathPrefix (KeyGenConfig path name) =
  pathStart <> name
  where
    pathStart = case endsWithSlash path of
      True -> path
      False -> path <> "/"

    endsWithSlash :: String -> Bool
    endsWithSlash ('/':[]) = True
    endsWithSlash (_:[]) = False
    endsWithSlash ([]) = False
    endsWithSlash (_:rest) = endsWithSlash rest

data CardanoCliCommand :: * -> * where
  CardanoCliCommand_QueryTip :: CardanoCliCommand (Either String Aeson.Value)
  CardanoCliCommand_GetProtocolParams :: CardanoCliCommand (Either String Api.ProtocolParameters)
  CardanoCliCommand_QueryUTXOs :: Api.AddressAny -> CardanoCliCommand (Either String (Api.UTxO Api.BabbageEra))

  -- | Generate private and public keys in a directory
  CardanoCliCommand_KeyGen :: KeyGenConfig -> CardanoCliCommand (Either String (FilePath, FilePath))
  CardanoCliCommand_BuildAddress :: FilePath -> CardanoCliCommand (Either String Api.AddressAny)

runCardanoCli :: (HasNodeInfo a, MonadIO m) => a -> CardanoCliCommand b -> m b
runCardanoCli a command = do
  ensureNodeSocket a
  case command of
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

    CardanoCliCommand_BuildAddress vkPath -> do
      runExceptT $ do
        addrStr <- ExceptT $ eitherReadProcess cp
        ExceptT $ pure $ maybeToEither "Failed to deserialize to Any Address" $ Api.deserialiseAddress Api.AsAddressAny . T.pack $ addrStr

    CardanoCliCommand_KeyGen kgc -> do
      -- NOTE: cardano-cli address key-gen doesn't fail if the folder doesn't exist, though no keys will be produced...
      liftIO $ createDirectoryIfMissing True $ kgc ^. keyGenConfig_path
      runExceptT $ do
        _ <- eitherReadProcess cp
        pure (path <> ".cardano.vk", path <> ".cardano.sk")
      where
        path = keyGenConfigPathPrefix kgc
  where
    cp = makeCliProcess (a ^. nodeInfo) command

buildAddress :: FilePath -> CardanoCliCommand (Either String Api.AddressAny)
buildAddress = CardanoCliCommand_BuildAddress

queryTip :: CardanoCliCommand (Either String Aeson.Value)
queryTip = CardanoCliCommand_QueryTip

getProtocolParameters :: CardanoCliCommand (Either String Api.ProtocolParameters)
getProtocolParameters = CardanoCliCommand_GetProtocolParams

queryUTxOs :: Api.AddressAny -> CardanoCliCommand (Either String (Api.UTxO Api.BabbageEra))
queryUTxOs = CardanoCliCommand_QueryUTXOs

keyGen :: KeyGenConfig -> CardanoCliCommand (Either String (FilePath, FilePath))
keyGen = CardanoCliCommand_KeyGen

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
        base [ "query"
             , "utxo"
             , "--address"
             , T.unpack $ Api.serialiseAddress addr
             , "--testnet-magic"
             , magic
             , "--out-file"
             , "/dev/stdout"
             ]
      CardanoCliCommand_BuildAddress vkPath ->
        base ["address", "build", "--verification-key-file", vkPath, "--testnet-magic", magic]
      CardanoCliCommand_KeyGen kgc ->
        base ["address"
             , "key-gen"
             , "--verification-key-file"
             , path <> ".cardano.vk"
             , "--signing-key-file"
             , path <> ".cardano.sk"
             ]
        where
          path = keyGenConfigPathPrefix kgc

    base = proc cardanoCliPath

    addSocketPath p =
      p { env = Just [("CARDANO_NODE_SOCKET_PATH", nodeInfo ^. nodeInfo_socketPath)]
        }

    magic = show $ nodeInfo ^. nodeInfo_magic

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b)  = Right b
maybeToEither a _ = Left a
