{-# LANGUAGE TemplateHaskell #-}

-- | 

module Hydra.Devnet where

import System.Which
import System.Directory
import System.Process

import Control.Monad
import Control.Monad.Log
import Control.Monad.IO.Class

import Control.Concurrent

import Data.Traversable
import Data.Foldable

import Data.Bool
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.String (fromString)

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

jqPath :: FilePath
jqPath = $(staticWhich "jq")

-- Create keys for cardano -- we need to keep track of the people created
-- and where their keys live.
-- Create keys for hydra
-- Seed wallets with some amounts
-- Create nodes per participant
-- Run network

-- A user like 'alice' that participates in the cardano devnet, and potenially a hydra-head
type ActorName = String
type CardanoNodeSocketPath = FilePath
type Lovelace = Int

-- | Cardano address
type Address = String

type TxInInfo = T.Text
type TxIn = T.Text
type TxId = T.Text

fuelMarkerDatumHash :: T.Text
fuelMarkerDatumHash = "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"

type HydraScriptTxId = T.Text

devnetNetworkId :: Int
devnetNetworkId = 42

publishReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraScriptTxId
publishReferenceScripts = do
  logMessage $ WithSeverity Informational $ "Publishing reference scripts ('νInitial' & 'νCommit')..."
  fmap (T.strip . T.pack) $ liftIO $ readCreateProcess cp ""
  where
    cp = (proc hydraNodePath [ "publish-scripts"
                              , "--network-id"
                              , show devnetNetworkId
                              , "--node-socket"
                              , "devnet/node.socket"
                              , "--cardano-signing-key"
                              , "devnet/credentials/faucet.sk"
                              ])
         -- TODO: is this node socket path in env needed?
         { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

writeEnv :: HydraScriptTxId -> IO ()
writeEnv hstxid = do
  writeFile ".env" $ "HYDRA_SCRIPTS_TX_ID=" <> T.unpack hstxid

seedDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraScriptTxId
seedDevnet = do
  logMessage $ WithSeverity Informational "Creating payment keys"
  for_ actors $ \(name, amount) -> do
    keysExist <- liftIO $ doesFileExist $ "credentials/" <> name <> ".vk"
    unless keysExist $ do
      logMessage $ WithSeverity Informational $ fromString $ fromString "Keys did not exist for actor " <> fromString name
      liftIO $ createActorPaymentKeys name
    seedActorFromFaucetAndWait name amount False
    seedActorFromFaucetAndWait name 100000000 True

  logMessage $ WithSeverity Informational "Publishing reference scripts"
  hstxid <- publishReferenceScripts

  logMessage $ WithSeverity Informational "Writing .env"
  liftIO $ writeEnv hstxid
  pure hstxid
  where
    -- FIXME(parenthetical): Pass in actors + keys (don't generate keys here)
    actors = [("alice", 1000000000), ("bob", 500000000), ("carol", 250000000)]

-- TODO(skylar): Currently always uses the devnet socket, but we likely don't want to use anything else
createActorPaymentKeys :: ActorName -> IO ()
createActorPaymentKeys name = do
  createDirectoryIfMissing False "credentials"
  _ <- readCreateProcess cp ""
  pure ()
  where
    cp = (proc cardanoCliPath [ "address"
                              , "key-gen"
                              , "--verification-key-file"
                              , "credentials/" <> name <> ".vk"
                              , "--signing-key-file"
                              , "credentials/" <> name <> ".sk"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

seedActorFromFaucetAndWait :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => ActorName -> Lovelace -> Bool -> m ()
seedActorFromFaucetAndWait name amount isFuel = do
  txin <- liftIO $ seedActorFromFaucet name amount isFuel
  waitForTxIn txin

-- | Send an amount in lovelace to the named actor
seedActorFromFaucet :: ActorName -> Lovelace -> Bool -> IO TxIn
seedActorFromFaucet name amount isFuel = do
  buildSeedTx name amount isFuel
  signSeedTx name
  txin <- txInput 0 <$> seedTxId name
  submitSeedTx name
  pure txin

waitForTxIn :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => TxIn -> m ()
waitForTxIn txin = do
  logMessage $ WithSeverity Informational $ "Waiting for utxo " <> pretty txin <> ".."
  liftIO $ waitFn
  where
    waitFn = do
      exists <- txInExists txin
      threadDelay 10000
      when (not exists) waitFn

txInExists :: TxIn -> IO Bool
txInExists txin = do
  result <- fmap (T.strip . T.pack) $ readCreateProcess cp "" >>= readProcess jqPath (pure $ ".\"" <> asStr <> "\"")
  pure $ case result of
    "null" -> False
    _ -> True
  where
    asStr = T.unpack txin
    cp = (proc cardanoCliPath [ "query"
                              , "utxo"
                              , "--tx-in"
                              , asStr
                              , "--out-file"
                              , "/dev/stdout"
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

txInput :: Int -> TxId -> TxIn
txInput index txid = txid <> "#" <> (T.pack . show) index

getFaucetTxIn :: IO TxIn
getFaucetTxIn = getFaucetAddress >>= getFirstTxIn

minTxLovelace :: Int
minTxLovelace = 857690

signSeedTx :: ActorName -> IO ()
signSeedTx name = do
  _ <- readCreateProcess cp ""
  pure ()
  where
    draftFile = "seed-" <> name <> ".draft"
    filename = "seed-" <> name <> ".signed"
    cp = (proc cardanoCliPath [ "transaction"
                              , "sign"
                              , "--tx-body-file"
                              , draftFile
                              , "--signing-key-file"
                              , "devnet/credentials/faucet.sk"
                              , "--out-file"
                              , filename
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

seedTxId :: ActorName -> IO TxId
seedTxId name =
  T.strip . T.pack <$> readCreateProcess cp ""
  where
    filename = "seed-" <> name <> ".signed"
    cp = (proc cardanoCliPath [ "transaction"
                              , "txid"
                              , "--tx-file"
                              , filename
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

submitSeedTx :: ActorName -> IO ()
submitSeedTx name = do
  _ <- readCreateProcess cp ""
  removeFile draftFile
  removeFile signedFile
  pure ()
  where
    draftFile = "seed-" <> name <> ".draft"
    signedFile = "seed-" <> name <> ".signed"
    cp = (proc cardanoCliPath [ "transaction"
                              , "submit"
                              , "--tx-file"
                              , signedFile
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

-- TODO(skylar): This should either create a temporary file or otherwise lock the file in the future...
buildSeedTx :: ActorName -> Lovelace -> Bool -> IO ()
buildSeedTx name amount isFuel = do
  exists <- doesFileExist filename
  when exists $ removeFile filename
  -- when (amount < minTxLovelace) $ error $ "Minmum required UTxO: Lovelace " <> show minTxLovelace
  faucet <- getFaucetAddress
  hash <- getFirstTxIn faucet
  addr <- getActorAddress name
  readCreateProcess (cp addr faucet (T.unpack hash)) ""
  pure ()
  where
    filename = "seed-" <> name <> ".draft"
    cp addr faucet hash = (proc cardanoCliPath $ filter (/= "") [ "transaction"
                              , "build"
                              , "--babbage-era"
                              , "--cardano-mode"
                              , "--change-address"
                              , faucet
                              , "--tx-in"
                              , hash
                              , "--tx-out"
                              , addr <> "+" <> show amount
                              ]
                              <> bool [] [ "--tx-out-datum-hash", T.unpack fuelMarkerDatumHash ] isFuel
                              <> 
                              [ "--out-file"
                              , filename
                              -- TODO(skylar): Why isn't this needed in seed-devnet??
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

-- | Convenience for getting faucet Output for seeding
getFirstTxIn :: Address -> IO TxIn
getFirstTxIn addr =
  readCreateProcess cp "" >>= readProcess jqPath ["-r", "keys[0]"] >>= \a -> pure $ T.strip $ T.pack a
  where
    cp = (proc cardanoCliPath [ "query"
                              , "utxo"
                              , "--address"
                              , addr
                              -- TODO(skylar): Why isn't this needed in seed-devnet??
                              , "--testnet-magic"
                              , "42"
                              , "--out-file"
                              , "/dev/stdout"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }


getActorAddress :: ActorName -> IO Address
getActorAddress name = readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , "credentials/" <> name <> ".vk"
                              -- TODO(skylar): Why isn't this needed in seed-devnet??
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

getFaucetAddress :: IO Address
getFaucetAddress = readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , "devnet/credentials/faucet.vk"
                              -- TODO(skylar): Why isn't this needed in seed-devnet??
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
