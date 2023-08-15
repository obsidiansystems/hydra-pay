{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Client where

import Control.Monad.Error.Class
import qualified Cardano.Ledger.BaseTypes as Ledger
import Options.Applicative
import Control.Exception
import System.Directory

import Data.Bifunctor

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Shelley

import qualified Data.Aeson as Aeson
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import HydraPay.Api
import HydraPay.Instance
import HydraPay.Config

data Command
  = Instance Text
  | Channel ChannelCommand

data ChannelCommand
  = Open Text Text Text
  | Status Text
  | Lock Text Double Text
  | Send Text Double Text
  | Submit Text Text FilePath
  | DoClose Text
  deriving (Show)

parseChannelCommand :: Parser Command
parseChannelCommand = fmap Channel $ subparser $
  command "status" (info parseStatus (progDesc "Get the status of a payment channel"))
  <> command "open" (info parseOpen (progDesc "Open a new payment channel"))
  <> command "lock" (info parseLock (progDesc "Lock funds in a payment channel"))
  <> command "send" (info parseSend (progDesc "Send ADA in a payment channel"))
  <> command "submit" (info parseSubmit (progDesc "Submit a signed transaction received from 'send'"))
  <> command "close" (info parseClose (progDesc "Close a payment channel"))

parseClose :: Parser ChannelCommand
parseClose = DoClose <$> argument str (metavar "<name>")

parseSubmit :: Parser ChannelCommand
parseSubmit = Submit <$> argument str (metavar "<name>") <*> argument str (metavar "<address>") <*> argument str (metavar "<file>")

parseSend :: Parser ChannelCommand
parseSend = Send <$> argument str (metavar "<name>") <*> argument auto (metavar "<amount-lovelace>") <*> argument str (metavar "<to-address>")

parseLock :: Parser ChannelCommand
parseLock = Lock <$> argument str (metavar "<name>") <*> argument auto (metavar "<amount-lovelace>") <*> argument str (metavar "<address>")

parseStatus :: Parser ChannelCommand
parseStatus = Status <$> argument str (metavar "<name>")

parseOpen :: Parser ChannelCommand
parseOpen = Open <$> argument str (metavar "<name>") <*> argument str (metavar "<address>") <*> argument str (metavar "<address>")

parseInstance :: Parser Command
parseInstance = Instance <$> argument str (metavar "<network-name>")

parseCommand :: Parser Command
parseCommand = subparser $
  command "instance" (info parseInstance (progDesc "Start a HydraPay instance, with the cardano node running on a certain network"))
  <> command "channel" (info parseChannelCommand (progDesc "Payment channel commands"))

mkCreate :: Text -> Text -> Text -> Either Text InstanceRequest
mkCreate name str1 str2 = do
  addr1 <- toAddressAny str1
  addr2 <- toAddressAny str2
  pure $ CreatePaymentChannel name addr1 addr2

mkSend :: Text -> Double -> Text -> Either Text InstanceRequest
mkSend name amount addrStr = do
  addr <- toAddressAny addrStr
  when (amount <= 0) $ throwError "You can't lock negative ADA"
  pure $ SendInChannel name addr lovelace
  where
    lovelace :: Int32
    lovelace = round $ amount * 1000000

mkLock :: Text -> Double -> Text -> Either Text InstanceRequest
mkLock name amount addrStr = do
  addr <- toAddressAny addrStr
  when (amount <= 0) $ throwError "You can't lock negative ADA"
  when (amount > 100) $ throwError "You can't lock more than 100 ADA"
  pure $ GetLock name addr lovelace
  where
    lovelace :: Int32
    lovelace = round $ amount * 1000000

mkSubmit :: (MonadError Text m, MonadIO m) => Text -> FilePath -> Text -> m InstanceRequest
mkSubmit name file addrStr = do
  exists <- liftIO $ doesFileExist file
  addr <- case addrResult of
    Right addr -> pure addr
    Left err -> throwError err
  case exists of
    False -> throwError $ "Couldn't find signed transaction file " <> T.pack file
    True -> do
      fileData <- liftIO $ T.readFile file
      pure $ SubmitInChannel name addr fileData

  where
    addrResult = toAddressAny addrStr

runClient :: IO ()
runClient = do
  let opts = info (parseCommand <**> helper) fullDesc
  cmd <- customExecParser (prefs showHelpOnEmpty) opts
  case cmd of
    Instance networkName -> case networkName of
      "mainnet" -> runInstance mainnetConfig
      "preprod" -> runInstance preprodConfig
      "preview" -> runInstance previewConfig
      "sanchonet" -> runInstance sanchonetConfig
      _ -> putStrLn "Please choose a network from the following options: mainnet | preview | preprod | sanchonet"
    Channel (Open name addrFirst addrSecond) -> do
      case mkCreate name addrFirst addrSecond of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Lock name addr amount) -> do
      case mkLock name addr amount of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Status name) -> clientRequest $ GetStatus name
    Channel (Send name addr amount) ->
      case mkSend name addr amount of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (Submit name addr file) -> do
      result <- runExceptT $ mkSubmit name file addr
      case result of
        Right req -> clientRequest req
        Left err -> putStrLn $ T.unpack err
    Channel (DoClose name) ->
      clientRequest (CloseChannel name)

clientRequest :: InstanceRequest -> IO ()
clientRequest req = do
  tryRes :: (Either SomeException ()) <- try $ WS.runClient "127.0.0.1" defaultPort "/" $ \conn -> do
    WS.sendDataMessage conn $ WS.Text (Aeson.encode req) Nothing
    payload <- WS.receiveDataMessage conn
    WS.sendClose conn $ ("We are done here" :: T.Text)
    let
      result = Aeson.decode $ case payload of
        WS.Text v _ -> v
        WS.Binary v -> v
    case result of
      Just resp -> T.putStrLn $ makeHumanReadable resp
      Nothing -> T.putStrLn "Failed to decode response from instance, is your instance the same version as this client?"
  case tryRes of
    Left reason -> putStrLn $ "Failed to connect to HydraPay instance at " <> show defaultPort <> " " <> show reason
    _ -> pure ()
  pure ()

-- | Get the network for a shelley address
getShelleyAddrNetwork :: Api.AddressAny -> Maybe Ledger.Network
getShelleyAddrNetwork = \case
  Api.AddressShelley (Shelley.ShelleyAddress network _ _) -> Just network
  Api.AddressByron _ -> Nothing

toAddressAny :: Text -> Either Text Api.AddressAny
toAddressAny a = case Api.deserialiseAddress Api.AsAddressAny (T.strip a) of
  Nothing -> Left "Invalid Address"
  Just addrAny -> return addrAny

toAddressAnyInNetwork :: Ledger.Network -> Text -> Either Text Api.AddressAny
toAddressAnyInNetwork network a = do
  addr <- first (const invalidAddrMsg) $ toAddressAny a
  case getShelleyAddrNetwork addr of
    Nothing -> Left "Invalid era"
    Just addrNetwork -> do
      when (addrNetwork /= network) $ Left invalidAddrMsg
      pure addr
  where
    invalidAddrMsg = T.unwords ["Invalid", T.pack (show network), "Address"]


nominalFuel :: Api.Lovelace
nominalFuel = 100000000

topUpLevel :: Api.Lovelace
topUpLevel = 60000000
