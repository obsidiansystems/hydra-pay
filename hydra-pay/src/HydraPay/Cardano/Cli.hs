{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Cardano.Cli where

import HydraPay.Types
import HydraPay.Utils
import HydraPay.Cardano.Node

import Data.Bifunctor
import System.Which
import System.Process
import System.Directory
import System.FilePath

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as Aeson

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api

-- | Provide a path and a name to use as a prefix for your signing and verification keys
data KeyGenTemplate = KeyGenTemplate
  { _keyGenTemplate_path :: FilePath
  , _keyGenTemplate_name :: FilePath
  }
  deriving (Eq, Show)

data KeyGenConfig
  = KeyGenConfigFromTemplate KeyGenTemplate
  -- ^ Use the generation template to create the paths
  | KeyGenConfig FilePath FilePath
  -- ^ Use these paths directly
  deriving (Eq, Show)

-- | Get the paths to the keys
keyGenConfigPaths :: KeyGenConfig -> (FilePath, FilePath)
keyGenConfigPaths = \case
  KeyGenConfigFromTemplate (KeyGenTemplate path _) -> (path, path)
  KeyGenConfig vk sk -> (takeDirectory vk, takeDirectory sk)

-- | Get the full filenames for each key
keyGenConfigFiles :: KeyGenConfig -> (FilePath, FilePath)
keyGenConfigFiles = \case
  KeyGenConfigFromTemplate (KeyGenTemplate path name) -> (path </> name <> ".vk", path </> name <> ".sk")
  KeyGenConfig vk sk -> (vk, sk)

keyGenFiles :: FilePath -> FilePath -> KeyGenConfig
keyGenFiles = KeyGenConfig

keyGenTemplate :: FilePath -> String -> KeyGenConfig
keyGenTemplate fp n = KeyGenConfigFromTemplate $ KeyGenTemplate fp n

makePrisms ''KeyGenConfig
makeLenses ''KeyGenTemplate

data CardanoCliCommand :: * -> * where
  CardanoCliCommand_QueryTip :: CardanoCliCommand (Either Text Aeson.Value)
  CardanoCliCommand_GetProtocolParams :: CardanoCliCommand (Either Text Api.ProtocolParameters)
  CardanoCliCommand_QueryUTXOs :: Api.AddressAny -> CardanoCliCommand (Either Text (Api.UTxO Api.BabbageEra))

  -- | Generate private and public keys in a directory
  CardanoCliCommand_KeyGen :: KeyGenConfig -> CardanoCliCommand (Either Text (FilePath, FilePath))
  CardanoCliCommand_BuildAddress :: FilePath -> CardanoCliCommand (Either Text Api.AddressAny)
  CardanoCliCommand_SubmitTxFile :: FilePath -> CardanoCliCommand (Either Text Text)
  CardanoCliCommand_GetTxId :: FilePath -> CardanoCliCommand (Either Text TxId)
  CardanoCliCommand_TxInExists :: TxInput -> CardanoCliCommand (Either Text Bool)

runCardanoCli :: (HasNodeInfo a, MonadIO m) => a -> CardanoCliCommand b -> m b
runCardanoCli a command = do
  ensureNodeSocket a
  case command of
    CardanoCliCommand_QueryTip -> do
      fmap (first T.pack) $ runExceptT $ do
        str <- ExceptT $ eitherReadProcess cp
        ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

    CardanoCliCommand_GetProtocolParams -> do
      fmap (first T.pack) $ runExceptT $ do
        str <- ExceptT $ eitherReadProcess cp
        ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

    CardanoCliCommand_QueryUTXOs _ -> do
      fmap (first T.pack) $ runExceptT $ do
        str <- ExceptT $ eitherReadProcess cp
        ExceptT $ pure $ Aeson.eitherDecode $ LBS.fromStrict $ B8.pack str

    CardanoCliCommand_BuildAddress _ -> do
      fmap (first T.pack) $ runExceptT $ do
        addrStr <- ExceptT $ eitherReadProcess cp
        ExceptT $ pure $ maybeToEither "Failed to deserialize to Any Address" $ Api.deserialiseAddress Api.AsAddressAny . T.pack $ addrStr

    CardanoCliCommand_SubmitTxFile _ -> do
      fmap (bimap T.pack T.pack) $ runExceptT $ do
        ExceptT $ eitherReadProcess cp

    CardanoCliCommand_GetTxId _ -> do
      fmap (bimap T.pack (TxId . T.pack)) $ runExceptT $ do
        ExceptT $ eitherReadProcess cp

    CardanoCliCommand_TxInExists _ -> do
      fmap (bimap T.pack parseResult) $ runExceptT $ do
        ExceptT $ eitherReadProcess cp
      where
        parseResult "{}" = False
        parseResult _ = True

    CardanoCliCommand_KeyGen kgc -> do
      -- NOTE: cardano-cli address key-gen doesn't fail if the folder doesn't exist, though no keys will be produced...
      liftIO $ createDirectoryIfMissing True vkPath
      liftIO $ createDirectoryIfMissing True skPath
      fmap (first T.pack) $ runExceptT $ do
        _ <- eitherReadProcess cp
        pure (vk, sk)
      where
        (vkPath, skPath) = keyGenConfigPaths kgc
        (vk, sk) = keyGenConfigFiles kgc
  where
    cp = makeCliProcess (a ^. nodeInfo) command

buildAddress :: FilePath -> CardanoCliCommand (Either Text Api.AddressAny)
buildAddress = CardanoCliCommand_BuildAddress

queryTip :: CardanoCliCommand (Either Text Aeson.Value)
queryTip = CardanoCliCommand_QueryTip

getProtocolParameters :: CardanoCliCommand (Either Text Api.ProtocolParameters)
getProtocolParameters = CardanoCliCommand_GetProtocolParams

queryUTxOs :: Api.AddressAny -> CardanoCliCommand (Either Text (Api.UTxO Api.BabbageEra))
queryUTxOs = CardanoCliCommand_QueryUTXOs

keyGen :: KeyGenConfig -> CardanoCliCommand (Either Text (FilePath, FilePath))
keyGen = CardanoCliCommand_KeyGen

submitTxFile :: FilePath -> CardanoCliCommand (Either Text Text)
submitTxFile = CardanoCliCommand_SubmitTxFile

getTxId :: FilePath -> CardanoCliCommand (Either Text TxId)
getTxId = CardanoCliCommand_GetTxId

txInExists :: TxInput -> CardanoCliCommand (Either Text Bool)
txInExists = CardanoCliCommand_TxInExists

makeCliProcess :: NodeInfo -> CardanoCliCommand a -> CreateProcess
makeCliProcess ni command =
  addSocketPath process
  where
    process = case command of
      CardanoCliCommand_TxInExists txinput ->
        base [ "query"
             , "utxo"
             , "--tx-in"
             , T.unpack $ txInputToText txinput
             , "--testnet-magic"
             , magic
             , "--out-file"
             , "/dev/stdout"
             ]

      CardanoCliCommand_QueryTip ->
        base ["query", "tip", "--testnet-magic", magic]

      CardanoCliCommand_GetProtocolParams ->
        base ["query", "protocol-parameters", "--testnet-magic", magic]

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
        base [ "address"
             , "key-gen"
             , "--verification-key-file"
             , vk
             , "--signing-key-file"
             , sk
             ]
        where
          (vk, sk) = keyGenConfigFiles kgc

      CardanoCliCommand_SubmitTxFile file ->
        base ["transaction", "submit", "--testnet-magic", magic, "--tx-file", file]

      CardanoCliCommand_GetTxId file ->
        base ["transaction", "txid", "--tx-file", file]

    base = proc cardanoCliPath

    addSocketPath p =
      p { env = Just [("CARDANO_NODE_SOCKET_PATH", ni ^. nodeInfo_socketPath)]
        }

    magic = show $ ni ^. nodeInfo_magic

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")
