{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | 

module Hydra.Devnet
  ( HydraScriptTxId
  , HydraKeyInfo(..)
  , SigningKey(..)
  , VerificationKey(..)
  , KeyPair(..)
  , mkKeyPair
  , SomeTx(..)
  , TxId
  , getCardanoAddress
  , seedAddressFromFaucetAndWait
  , getReferenceScripts
  , publishReferenceScripts
  , queryAddressUTXOs
  , buildSignedTx
  , generateKeys
  , generateKeysIn
  , cardanoNodePath
  , hydraNodePath
  , prepareDevnet
  , seedTestAddresses
  , getTestAddressKeys
  , devnetMagic
  , minTxLovelace
  , addressesPath
  , getTempPath
  , getTempPath'
  , cardanoCliPath
  , submitTx
  , waitForTxIn
  , txInput
  , buildSignedHydraTx
  , generateCardanoKeys
  , getTipSlotNo
  , transferAmount
  , transferAll
  , getAllLovelaceUtxos
  , getDevnetAddress
  , getDevnetAddresses
  , devnetFaucetKeys
  , cardanoDevnetNodeInfo
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
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Paths

import qualified Data.ByteString.Lazy.Char8 as BS

import Hydra.Types
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Maybe (fromMaybe)

import Data.Traversable
import CardanoNodeInfo

import Common.Helpers

devnetMagic :: Int
devnetMagic = 42

cardanoDevnetNodeInfo :: CardanoNodeInfo
cardanoDevnetNodeInfo = CardanoNodeInfo (TestNet devnetMagic) "devnet/node.socket" "devnet/devnet-protocol-parameters.json" "devnet/genesis-shelley.json"

devnetFaucetKeys :: KeyPair
devnetFaucetKeys = mkKeyPair "devnet/credentials/faucet.sk" "devnet/credentials/faucet.vk"

prepareDevnet :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m ()
prepareDevnet = do
  output <- liftIO $ readCreateProcess (shell "[ -d devnet ] || ./config/backend/prepare-devnet.sh") ""
  when (null output) $ logMessage $ WithSeverity Informational $ pretty $ T.pack output

addressesPath :: FilePath
addressesPath = "devnet/addresses"

seedTestAddresses :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> KeyPair -> Int -> m ()
seedTestAddresses cninf faucetKeys amount = do
  exists <- liftIO $ doesFileExist path
  unless exists $ do
    seededAddresses <- for [1 .. amount] $ \n -> do
      keypair <- generateCardanoKeys ("addr_" <> show n)
      addr <- liftIO $ getCardanoAddress cninf $ _verificationKey keypair
      void $ seedAddressFromFaucetAndWait cninf faucetKeys addr (ada 10000) False
      pure addr
    liftIO $ T.writeFile path $ T.intercalate "\n" seededAddresses
  where
    path = addressesPath

-- TODO: delete this function in favor of keys-as-values
getTestAddressKeys :: Address -> IO (Maybe KeyPair)
getTestAddressKeys addr = do
  contents <- flip zip [1..] . T.lines <$> T.readFile path
  pure $ fmap mkKeypair . lookup addr $ contents
  where
    mkKeypair n = KeyPair (SigningKey $ root <> "sk") (VerificationKey $ root <> "vk")
      where
        root = "addr_" <> show n <> ".cardano."
    path = addressesPath

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

newtype SigningKey = SigningKey
  { getSigningKeyFilePath :: FilePath }
  deriving (Eq, Show, Read)

newtype VerificationKey = VerificationKey
  { getVerificationKeyFilePath :: FilePath }
  deriving (Eq, Show, Read)

data KeyPair = KeyPair
  { _signingKey :: SigningKey
  , _verificationKey :: VerificationKey
  }
  deriving (Show, Read)

mkKeyPair :: FilePath -> FilePath -> KeyPair
mkKeyPair spath vpath = KeyPair (SigningKey spath) (VerificationKey vpath)

data HydraKeyInfo = HydraKeyInfo
  { _cardanoKeys :: KeyPair
  , _hydraKeys :: KeyPair
  }
  deriving (Show, Read)

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
  pure $ mkKeyPair [i|#{path}.cardano.sk|] [i|#{path}.cardano.vk|]

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
  pure $ mkKeyPair [i|#{path}.hydra.sk|] [i|#{path}.hydra.vk|]

-- | Publishes the reference scripts if they don't exist on chain, will read them otherwise
getReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => FilePath -> SigningKey -> m HydraScriptTxId
getReferenceScripts scriptPath sk = do
  exists <- liftIO $ doesFileExist scriptPath
  case exists of
    True -> liftIO $ T.readFile scriptPath
    False -> do
      scripts <- publishReferenceScripts sk
      liftIO $ T.writeFile scriptPath scripts
      pure scripts

-- TODO: Make this generic over node and signing key?
publishReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => SigningKey -> m HydraScriptTxId
publishReferenceScripts sk = do
  logMessage $ WithSeverity Informational "Publishing reference scripts ('νInitial' & 'νCommit')..."
  fmap (T.strip . T.pack) $ liftIO $ readCreateProcess cp ""
  where
    cp = proc hydraNodePath [ "publish-scripts"
                              , "--network-id"
                              , show devnetNetworkId
                              , "--node-socket"
                              , "devnet/node.socket"
                              , "--cardano-signing-key"
                              , getSigningKeyFilePath sk
                              ]


waitForTxIn :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> TxIn -> m ()
waitForTxIn cninf txin = do
  logMessage $ WithSeverity Informational $ "Waiting for utxo " <> pretty txin <> ".."
  liftIO waitFn
  where
    waitFn = do
      exists <- txInExists cninf txin
      threadDelay 10000
      unless exists waitFn


cardanoNodeArgs :: CardanoNodeInfo -> [String]
cardanoNodeArgs cninf = ["--testnet-magic", show . _testNetMagic . _nodeType $ cninf]

txInExists :: CardanoNodeInfo -> TxIn -> IO Bool
txInExists cninf txin = do
  result <- fmap (T.strip . T.pack) $ readCreateProcess cp "" >>= readProcess jqPath (pure $ ".\"" <> asStr <> "\"")
  pure $ case result of
    "null" -> False
    _ -> True
  where
    asStr = T.unpack txin
    cp =
      ( proc cardanoCliPath $
          [ "query",
            "utxo",
            "--tx-in",
            asStr,
            "--out-file",
            "/dev/stdout"
          ]
            <> cardanoNodeArgs cninf
      )
        { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)]
        }
txInput :: Int -> TxId -> TxIn
txInput index txid = txid <> "#" <> (T.pack . show) index

minTxLovelace :: Int
minTxLovelace = 857690

queryAddressUTXOs :: MonadIO m => CardanoNodeInfo -> Address -> m WholeUTXO
queryAddressUTXOs cninf addr = liftIO $ do
  let queryProc =
        (proc cardanoCliPath $ [ "query"
                             , "utxo"
                             , "--address"
                             , T.unpack addr
                             , "--out-file"
                             , "/dev/stdout"
                             ]
            <> cardanoNodeArgs cninf)
        { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }
  str <- readCreateProcess queryProc ""
  pure $ fromMaybe mempty $ decode $ BS.pack str


getTempPath' :: IO FilePath
getTempPath' = snd <$> getTempPath

getTempPath :: IO (UUID, FilePath)
getTempPath = do
  createDirectoryIfMissing True "tmp"
  uid <- UUIDV4.nextRandom
  pure . (uid,) . ("tmp/" <>) . UUID.toString $ uid

data SomeTx = SomeTx
  { _tx_ins :: [TxIn]
  , _tx_outAddr :: Address
  , _tx_outAmount :: Lovelace
  , _tx_changeAddr :: Address
  , _tx_outDatumHash :: Maybe T.Text
  }

buildSignedTx :: CardanoNodeInfo -> SigningKey -> SomeTx -> IO (FilePath, TxId)
buildSignedTx nodeInfo signingKey tx = do
  draftTx <- buildDraftTx nodeInfo tx
  signedTx <- signTx nodeInfo signingKey draftTx
  txid <- txIdFromSignedTx signedTx
  pure (signedTx,txid)

-- | Convenience for getting faucet Output for seeding
getFirstTxIn :: CardanoNodeInfo -> Address -> IO TxIn
getFirstTxIn cninf addr =
  readCreateProcess cp "" >>= readProcess jqPath ["-r", "keys[0]"] >>= \a -> pure $ T.strip $ T.pack a
  where
    cp = (proc cardanoCliPath $
                              [ "query"
                              , "utxo"
                              , "--address"
                              , T.unpack addr
                              , "--out-file"
                              , "/dev/stdout"
                              ]
                              <> cardanoNodeArgs cninf
        ) { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }


getCardanoAddress :: CardanoNodeInfo -> VerificationKey -> IO Address
getCardanoAddress cninf keyPath =
  T.pack <$> readCreateProcess cp ""
  where
    cp = (proc cardanoCliPath $ [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , getVerificationKeyFilePath keyPath
                              ]
               <> cardanoNodeArgs cninf
         ) { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }


-- | Returns Nothing if the address doesn't have enough funds to cover fees.
transferAll :: () => CardanoNodeInfo -> SigningKey -> Address -> Address -> IO (Maybe TxIn)
transferAll cninf signingKey fromAddr destAddr = do
  txins <- getAllLovelaceUtxos cninf fromAddr
  transferAmount cninf signingKey (fmap toInteger txins) destAddr True Nothing
  

seedAddressFromFaucetAndWait :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> KeyPair -> Address -> Lovelace -> Bool -> m TxIn
seedAddressFromFaucetAndWait cninf faucetKeys addr amount isFuel = do
  txin <- liftIO $ seedAddressFromFaucet cninf faucetKeys addr amount isFuel
  waitForTxIn cninf txin
  pure txin

-- | Send an amount in lovelace to the named actor
seedAddressFromFaucet :: CardanoNodeInfo -> KeyPair -> Address -> Lovelace -> Bool -> IO TxIn
seedAddressFromFaucet cninf (KeyPair faucetsk faucetvk) addr amount isFuel = do
  draftTx <- buildSeedTxForAddress cninf faucetvk addr amount isFuel
  signedTx <- signTx cninf faucetsk draftTx
  txin <- txInput 0 <$> txIdFromSignedTx signedTx
  void $ submitTx cninf signedTx
  pure txin

buildDraftTx :: () => CardanoNodeInfo -> SomeTx -> IO FilePath
buildDraftTx cninf tx = do
  filename <- getTempPath'
  _ <- readCreateProcess ((proc cardanoCliPath $ filter (/= "")
                              [ "transaction"
                              , "build"
                              , "--babbage-era"
                              , "--cardano-mode"
                              , "--change-address"
                              , T.unpack (_tx_changeAddr tx)
                              ]
                              <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) $ _tx_ins tx)
                              <>
                              [ "--tx-out"
                              , T.unpack (_tx_outAddr tx) <> "+" <> show (_tx_outAmount tx)
                              ]
                              <> maybe [] (\h -> [ "--tx-out-datum-hash", T.unpack h ]) (_tx_outDatumHash tx)
                              <>
                              [ "--out-file"
                              , filename
                              ]
                              <> cardanoNodeArgs cninf)
                              { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
                           ""
  pure filename

buildRawEmptyTx :: () => CardanoNodeInfo -> [TxIn] -> [Address] -> IO FilePath
buildRawEmptyTx cninf txins outAddrs = do
  filename <- getTempPath'
  _ <- readCreateProcess ((proc cardanoCliPath $ filter (/= "")
                              [ "transaction"
                              , "build-raw"
                              ]
                              <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) $ txins)
                              <>
                              concatMap (\addr -> [ "--tx-out", T.unpack addr <> "+0" ]) outAddrs
                              <>
                              [ "--fee", "0"
                              , "--invalid-hereafter", "0"
                              , "--out-file", filename
                              ])
                              { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
                           ""
  pure filename

calculateMinFees :: () => CardanoNodeInfo -> FilePath -> Int -> Int -> IO Lovelace
calculateMinFees cninf rawEmptyTx inCount outCount = do
  out <- readCreateProcess ((proc cardanoCliPath $ filter (/= "")
                              [ "transaction"
                              , "calculate-min-fee"
                              , "--tx-body-file", rawEmptyTx
                              , "--tx-in-count", show inCount
                              , "--tx-out-count", show outCount
                              , "--witness-count", "1"
                              , "--byron-witness-count", "0"
                              , "--protocol-params-file", _nodeLedgerProtocolParameters cninf
                              ]
                              <> cardanoNodeArgs cninf)
                              { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
                           ""
  pure $ read (head (words out))

getTipSlotNo :: CardanoNodeInfo -> IO Integer
getTipSlotNo cninf = do
  out1 <- readCreateProcess
          ((proc cardanoCliPath $ ["query", "tip"] <> cardanoNodeArgs cninf)
           { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
          ""
  read <$> readProcess jqPath (pure $ ".\"slot\"") out1

getAllLovelaceUtxos :: CardanoNodeInfo -> Address -> IO (Map TxIn Int)
getAllLovelaceUtxos cninf fromAddr = do
  allUtxos <- queryAddressUTXOs cninf fromAddr
  pure $ Map.mapMaybe (Map.lookup "lovelace" . value) $ allUtxos

-- | Returns Nothing if there are not enough funds available.
transferAmount :: CardanoNodeInfo -> SigningKey -> Map TxIn Lovelace -> Address -> Bool -> Maybe Lovelace -> IO (Maybe TxId)
transferAmount cninf signingKey utxos toAddr minusFee maybeAmount = do
  let txins = Map.keys utxos
  let fullAmount :: Lovelace = sum . Map.elems $ utxos
  emptyTx <- buildRawEmptyTx cninf txins [toAddr]
  fee <- calculateMinFees cninf emptyTx (Map.size utxos) 1
  let amount = fromMaybe fullAmount maybeAmount
  if (if minusFee
      then fee >= amount || amount > fullAmount
      else fee + amount >= fullAmount)
    then pure Nothing
    else Just <$> do
      tipSlotNo <- getTipSlotNo cninf
      submitTx cninf
        =<< signTx cninf signingKey
        =<< buildRawTx cninf txins (Map.singleton toAddr (bool (+) (-) minusFee fullAmount fee)) (tipSlotNo + 200) fee

buildRawTx :: CardanoNodeInfo -> [TxIn] -> Map Address Lovelace -> Integer -> Lovelace -> IO FilePath
buildRawTx cninf txins outAmounts invalidAfter fee = do
  outFile <- getTempPath'
  _ <- readCreateProcess
    ((proc cardanoCliPath $ ["transaction", "build-raw"
                            ]
                            <> concatMap (\txin -> ["--tx-in", T.unpack txin]) txins
                            <> concatMap (\(addr,amount) -> [ "--tx-out", [i|#{addr}+#{amount}|] ]) (Map.toList outAmounts)
                            <> [ "--invalid-hereafter", show invalidAfter
                               , "--fee", show fee
                               , "--out-file", outFile
                               ]
     )
     { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
    ""
  pure outFile

buildSeedTxForAddress :: CardanoNodeInfo -> VerificationKey -> Address -> Lovelace -> Bool -> IO DraftTx
buildSeedTxForAddress cninf faucetvk addr amount isFuel = do
  faucetAddr <- getCardanoAddress cninf faucetvk
  hash <- getFirstTxIn cninf faucetAddr
  buildDraftTx cninf $ SomeTx
      { _tx_ins = [hash],
        _tx_outAddr = addr,
        _tx_outAmount = amount,
        _tx_changeAddr = faucetAddr,
        _tx_outDatumHash = fuelMarkerDatumHash <$ guard isFuel
      }

-- | Sign a transaction and a path containing it.
signTx :: CardanoNodeInfo -> SigningKey -> DraftTx -> IO SignedTx
signTx cninf sk draftFile = do
  outFile <- getTempPath'
  let cp =
        ( proc cardanoCliPath $
            [ "transaction",
              "sign",
              "--tx-body-file",
              draftFile,
              "--signing-key-file",
              getSigningKeyFilePath sk,
              "--out-file",
              outFile
            ]
              <> cardanoNodeArgs cninf
        )
          { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)]
          }
  _ <- readCreateProcess cp ""
  pure outFile

txIdFromSignedTx :: SignedTx -> IO TxId
txIdFromSignedTx filename =
  T.strip . T.pack <$> readCreateProcess cp ""
  where
    cp =
      ( proc cardanoCliPath $
          [ "transaction",
            "txid",
            "--tx-file",
            filename
          ]
      )

submitTx :: CardanoNodeInfo -> SignedTx -> IO TxId
submitTx cninf signedFile = do
  txid <- txInput 0 <$> txIdFromSignedTx signedFile
  _ <- readCreateProcess cp ""
  pure txid
  where
    cp =
      ( proc cardanoCliPath $
          [ "transaction",
            "submit",
            "--tx-file",
            signedFile
          ]
          <> cardanoNodeArgs cninf
      )
        { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)]
        }

-- | Return (transaction id, signed tx json text).
buildSignedHydraTx :: SigningKey -> Address -> Address -> Map TxIn Lovelace -> Lovelace -> IO (T.Text, String)
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
  txid <- readCreateProcess
    (proc cardanoCliPath
      [ "transaction"
      , "txid"
      , "--tx-body-file"
      , txBodyPath
      ])
    ""
  (T.strip . T.pack $ txid,) <$> readCreateProcess
    (proc cardanoCliPath
      [ "transaction"
      , "sign"
      , "--tx-body-file"
      , txBodyPath
      , "--signing-key-file"
      , getSigningKeyFilePath signingKey
      , "--out-file"
      , "/dev/stdout"
      ])
    ""

getDevnetAddresses :: [Int] -> IO (Maybe [Address])
getDevnetAddresses is = do
  addrs <- zip [1..] . T.lines . T.pack <$> readFile addressesPath
  pure $ for is (flip lookup addrs)

getDevnetAddress :: Int -> IO (Maybe Address)
getDevnetAddress i = do
  addrs <- getDevnetAddresses [i]
  pure $ join $ headMay <$> addrs
