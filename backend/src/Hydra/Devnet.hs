{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | 

module Hydra.Devnet
  ( HydraScriptTxId
  , HydraKeyInfo(..)
  , SigningKey
  , KeyPair(..)
  , getCardanoAddress
  , seedAddressFromFaucetAndWait
  , publishReferenceScripts
  , queryAddressUTXOs
  , buildSignedHydraTx
  , generateKeys
  , generateKeysIn
  , cardanoNodePath
  , hydraNodePath
  , prepareDevnet
  , devnetMagic
  , minTxLovelace

  -- TODO(skylar) Temporary
  , getTempPath
  , cardanoCliPath
  )

where

import System.Which
import System.Directory
import System.Process

import Control.Monad
import Control.Monad.Log
import Control.Monad.IO.Class
import Data.Aeson

import Control.Concurrent
import Data.Map (Map)

import Data.Bool
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Paths

import qualified Data.ByteString.Lazy.Char8 as BS

import Hydra.Types
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Maybe (fromMaybe)


devnetMagic :: Int
devnetMagic = 42

prepareDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ()
prepareDevnet = do
  output <- liftIO $ readCreateProcess (shell "[ -d devnet ] || ./demo/prepare-devnet.sh") ""
  when (null output) $ logMessage $ WithSeverity Informational $ pretty $ T.pack output

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

jqPath :: FilePath
jqPath = $(staticWhich "jq")

type TxId = T.Text

type HydraScriptTxId = T.Text

type DraftTx = FilePath
type SignedTx = FilePath

devnetNetworkId :: Int
devnetNetworkId = 42

generateKeys :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m) => m HydraKeyInfo
generateKeys = do
  basePath <- liftIO getTempPath'
  HydraKeyInfo <$> generateCardanoKeys basePath <*> generateHydraKeys basePath

generateKeysIn :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m) => FilePath ->  m HydraKeyInfo
generateKeysIn fp =
  HydraKeyInfo <$> generateCardanoKeys fp <*> generateHydraKeys fp

type SigningKey = FilePath
type VerificationKey = FilePath

data KeyPair = KeyPair
  { _signingKey :: SigningKey
  , _verificationKey :: VerificationKey
  }
  deriving (Show,Read)


data HydraKeyInfo = HydraKeyInfo
  { _cardanoKeys :: KeyPair
  , _hydraKeys :: KeyPair
  }
  deriving (Show,Read)

-- | Generate Cardano keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.cardano.{vk,sk}" keys being
-- written.
generateCardanoKeys :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => String -> m KeyPair
generateCardanoKeys path = do
  output <- liftIO $
    readCreateProcess
    (proc cardanoCliPath [ "address"
                         , "key-gen"
                         , "--verification-key-file"
                         , [i|#{path}.cardano.vk|]
                         , "--signing-key-file"
                         , [i|#{path}.cardano.sk|]
                         ])
    ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  pure $ KeyPair [i|#{path}.cardano.sk|] [i|#{path}.cardano.vk|]

-- | Generate Hydra keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.hydra.{vk,sk}" keys being
-- written.
generateHydraKeys :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => String -> m KeyPair
generateHydraKeys path = do
  output <- liftIO $
    readCreateProcess
    (proc hydraToolsPath [ "gen-hydra-key"
                         , "--output-file"
                         , [i|#{path}.hydra|]
                         ])
    ""
  logMessage $ WithSeverity Informational $ pretty $ T.pack output
  pure $ KeyPair [i|#{path}.hydra.sk|] [i|#{path}.hydra.vk|]


publishReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraScriptTxId
publishReferenceScripts = do
  logMessage $ WithSeverity Informational "Publishing reference scripts ('νInitial' & 'νCommit')..."
  fmap (T.strip . T.pack) $ liftIO $ readCreateProcess cp ""
  where
    cp = proc hydraNodePath [ "publish-scripts"
                              , "--network-id"
                              , show devnetNetworkId
                              , "--node-socket"
                              , "devnet/node.socket"
                              , "--cardano-signing-key"
                              , "devnet/credentials/faucet.sk"
                              ]


waitForTxIn :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => TxIn -> m ()
waitForTxIn txin = do
  logMessage $ WithSeverity Informational $ "Waiting for utxo " <> pretty txin <> ".."
  liftIO waitFn
  where
    waitFn = do
      exists <- txInExists txin
      threadDelay 10000
      unless exists waitFn

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

-- TODO: use this in checks?
minTxLovelace :: Int
minTxLovelace = 857690

queryAddressUTXOs :: MonadIO m => Address -> m WholeUTXO
queryAddressUTXOs addr = liftIO $ do
  let queryProc =
        (proc cardanoCliPath [ "query"
                             , "utxo"
                             , "--address"
                             , T.unpack addr
                             , "--testnet-magic"
                             , "42"
                             , "--out-file"
                             , "/dev/stdout"
                             ])
        { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
  str <- readCreateProcess queryProc ""
  pure $ fromMaybe mempty $ decode $ BS.pack str


getTempPath' :: IO FilePath
getTempPath' = snd <$> getTempPath

getTempPath :: IO (UUID, FilePath)
getTempPath = do
  createDirectoryIfMissing True "tmp"
  uid <- UUIDV4.nextRandom
  pure . (uid,) . ("tmp/" <>) . UUID.toString $ uid

-- TODO(skylar): Check lovelace vs the full amount!
buildSignedHydraTx :: SigningKey -> Address -> Address -> Map TxIn Lovelace -> Lovelace -> IO String
buildSignedHydraTx signingKey fromAddr toAddr txInAmounts amount = do
  let fullAmount = sum txInAmounts
  txBodyPath <- snd <$> getTempPath
  void $ readCreateProcess (proc cardanoCliPath
                       ([ "transaction"
                        , "build-raw"
                        , "--babbage-era"
                        ]
                        <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) . Map.keys $ txInAmounts)
                        <>
                        [ "--tx-out"
                        , [i|#{toAddr}+#{amount}|]
                        , "--tx-out"
                        , [i|#{fromAddr}+#{fullAmount - amount}|]
                        , "--fee"
                        , "0"
                        , "--out-file"
                        , txBodyPath
                        ]))
    ""
  readCreateProcess
    (proc cardanoCliPath
      [ "transaction"
      , "sign"
      , "--tx-body-file"
      , txBodyPath
      , "--signing-key-file"
      , signingKey
      , "--out-file"
      , "/dev/stdout"
      ])
    ""
--    { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

-- | Convenience for getting faucet Output for seeding
getFirstTxIn :: Address -> IO TxIn
getFirstTxIn addr =
  readCreateProcess cp "" >>= readProcess jqPath ["-r", "keys[0]"] >>= \a -> pure $ T.strip $ T.pack a
  where
    cp = (proc cardanoCliPath [ "query"
                              , "utxo"
                              , "--address"
                              , T.unpack addr
                              , "--testnet-magic"
                              , "42"
                              , "--out-file"
                              , "/dev/stdout"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }


getCardanoAddress :: VerificationKey -> IO Address
getCardanoAddress keyPath =
  T.pack <$> readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , keyPath
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }


getFaucetAddress :: IO Address
getFaucetAddress = T.pack <$> readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , "devnet/credentials/faucet.vk"
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

seedAddressFromFaucetAndWait :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => Address -> Lovelace -> Bool -> m TxIn
seedAddressFromFaucetAndWait addr amount isFuel = do
  txin <- liftIO $ seedAddressFromFaucet addr amount isFuel
  waitForTxIn txin
  pure txin

-- | Send an amount in lovelace to the named actor
seedAddressFromFaucet :: Address -> Lovelace -> Bool -> IO TxIn
seedAddressFromFaucet addr amount isFuel = do
  draftTx <- buildSeedTxForAddress addr amount isFuel
  signedTx <- signSeedTx' draftTx
  txin <- txInput 0 <$> seedTxIdFromSignedTx signedTx
  submitTx signedTx
  pure txin


buildSeedTxForAddress :: Address -> Lovelace -> Bool -> IO DraftTx
buildSeedTxForAddress addr amount isFuel = do
  filename <- getTempPath'
  -- when (amount < minTxLovelace) $ error $ "Minmum required UTxO: Lovelace " <> show minTxLovelace
  let cp faucet hash = (proc cardanoCliPath $ filter (/= "")
                              [ "transaction"
                              , "build"
                              , "--babbage-era"
                              , "--cardano-mode"
                              , "--change-address"
                              , faucet
                              , "--tx-in"
                              , hash
                              , "--tx-out"
                              , T.unpack addr <> "+" <> show amount
                              ]
                              <> bool [] [ "--tx-out-datum-hash", T.unpack fuelMarkerDatumHash ] isFuel
                              <>
                              [ "--out-file"
                              , filename
                              , "--testnet-magic"
                              , "42"
                              ])
                            { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
  faucet <- getFaucetAddress
  hash <- getFirstTxIn faucet
  _ <- readCreateProcess (cp (T.unpack faucet) (T.unpack hash)) ""
  pure filename


signSeedTx' :: DraftTx -> IO SignedTx
signSeedTx' draftFile = do
  outFile <- getTempPath'
  let cp = (proc cardanoCliPath [ "transaction"
                                , "sign"
                                , "--tx-body-file"
                                , draftFile
                                , "--signing-key-file"
                                , "devnet/credentials/faucet.sk"
                                , "--out-file"
                                , outFile
                                , "--testnet-magic"
                                , "42"
                                ])
           { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
  _ <- readCreateProcess cp ""
  pure outFile

seedTxIdFromSignedTx :: SignedTx -> IO TxId
seedTxIdFromSignedTx filename =
  T.strip . T.pack <$> readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath [ "transaction"
                              , "txid"
                              , "--tx-file"
                              , filename
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }

submitTx :: SignedTx -> IO ()
submitTx signedFile = do
  _ <- readCreateProcess cp ""
  pure ()
  where
    cp = (proc cardanoCliPath [ "transaction"
                              , "submit"
                              , "--tx-file"
                              , signedFile
                              , "--testnet-magic"
                              , "42"
                              ]) { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
