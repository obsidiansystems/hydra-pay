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
  , publishReferenceScripts
  , queryAddressUTXOs
  , buildSignedTx
  , generateKeys
  , generateKeysIn
  , cardanoNodePath
  , cardanoSubmitApiPath
  , hydraNodePath
  , prepareDevnet
  , seedTestAddresses
  , getTestAddressKeys
  , devnetMagic
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
  , writeProtocolParameters
  )

where

import System.Exit (ExitCode(..))

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

import Control.Monad.Except

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
  _ <- liftIO $ readCreateProcess (shell [i| [ -d devnet ] || #{livedocDevnetScriptPath}|]) ""
  logInfo "Prepared Devnet, running..."
  pure ()

addressesPath :: FilePath
addressesPath = "devnet/addresses"

keysPath :: FilePath
keysPath = "devnet/keys"

-- | Generate Cardano keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.cardano.{phrase,prv,vk,sk}" keys being
-- written.
generateCardanoKeys :: MonadIO m => String -> m (Either String KeyPair)
generateCardanoKeys prefix = liftIO $ runExceptT $ do
  -- Generate phrase
  phrase <- ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoWalletPath [ "recovery-phrase"
                            , "generate"
                            ]
    ) ""
  -- Write phrase
  liftIO $ writeFile (prefix <> ".cardano.phrase") phrase

  -- Generate cardano-address extended key file from phrase
  privateKey <- ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoWalletPath ["key", "from-recovery-phrase", "Shelley"]) phrase
  paymentKey <- ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoWalletPath ["key", "child", "1852H/1815H/0H/0/0"]) privateKey
  let
    cardanoAddressFormatSigningKeyFile = prefix <> ".prv"
    signingKeyFile = prefix <> ".cardano.sk"
    verificationKeyFile = prefix <> ".cardano.vk"

  -- Write out cardano-address extended key file to be consumed by cardano-cli conversion
  liftIO $ writeFile cardanoAddressFormatSigningKeyFile paymentKey

   -- Convert cardano-address extended signing key to shelley format
  ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoCliPath [ "key"
                           , "convert-cardano-address-key"
                           , "--shelley-payment-key"
                           , "--signing-key-file"
                           , cardanoAddressFormatSigningKeyFile
                           , "--out-file"
                           , signingKeyFile]) privateKey
  ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoCliPath [ "key"
                         , "verification-key"
                         , "--signing-key-file"
                         , signingKeyFile
                         , "--verification-key-file"
                         , verificationKeyFile]) ""
  pure $ KeyPair (SigningKey signingKeyFile) (VerificationKey verificationKeyFile)

seedTestAddresses :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> KeyPair -> Int -> m ()
seedTestAddresses cninf faucetKeys amount = do
  exists <- liftIO $ doesFileExist addressesPath
  unless exists $ do
    -- Remove the Cardano keys for the devnet
    liftIO $ do
      -- NOTE removePathForcibly does nothing if the path doesn't exist,
      -- and doesn't throw exceptions when the path contains files
      _ <- removePathForcibly keysPath
      createDirectory keysPath
    result <- for [1 .. amount] $ \n -> runExceptT $ do
      keypair <- ExceptT $ generateCardanoKeys $ keysPath <> "/addr_" <> show n
      addr <- ExceptT $ liftIO $ getCardanoAddress cninf $ _verificationKey keypair
      void $ seedAddressFromFaucetAndWait cninf faucetKeys addr (ada 10000) False
      pure addr
    case sequenceA result of
      Right seededAddresses ->
        liftIO $ T.writeFile addressesPath $ T.intercalate "\n" $ fmap unAddress seededAddresses
      Left err -> logError $ pretty err

getTestAddressKeys :: Address -> IO (Maybe KeyPair)
getTestAddressKeys addr = do
  exists <- doesFileExist addressesPath
  case exists of
    False -> pure Nothing
    True -> do
      contents <- flip zip [(1 :: Integer)..] . fmap UnsafeToAddress . T.lines <$> T.readFile addressesPath
      pure $ fmap mkKeypair . lookup addr $ contents
  where
    mkKeypair n = KeyPair (SigningKey $ root <> "sk") (VerificationKey $ root <> "vk")
      where
        root = keysPath <> "/" <> "addr_" <> show n <> ".cardano."

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")

cardanoWalletPath :: FilePath
cardanoWalletPath = $(staticWhich "cardano-wallet")

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

cardanoSubmitApiPath :: FilePath
cardanoSubmitApiPath = $(staticWhich "cardano-submit-api")

jqPath :: FilePath
jqPath = $(staticWhich "jq")

type HydraScriptTxId = T.Text

type DraftTx = FilePath
type SignedTx = FilePath

generateKeys :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m) => m (Either String HydraKeyInfo)
generateKeys = do
  basePath <- liftIO getTempPath'
  runExceptT $ HydraKeyInfo <$> (ExceptT $ generateCardanoKeys basePath) <*> (ExceptT $ generateHydraKeys basePath)

generateKeysIn :: (MonadLog (WithSeverity (Doc ann)) m, MonadIO m) => FilePath -> m (Either String HydraKeyInfo)
generateKeysIn fp = runExceptT $
  HydraKeyInfo <$> (ExceptT $ generateCardanoKeys fp) <*> (ExceptT $ generateHydraKeys fp)

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

-- | Generate Hydra keys. Calling with an e.g. "my/keys/alice"
-- argument results in "my/keys/alice.hydra.{vk,sk}" keys being
-- written.
generateHydraKeys :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => String -> m (Either String KeyPair)
generateHydraKeys path = do
  (exitCode, output, stderr) <- liftIO $
    readCreateProcessWithExitCode
    (proc hydraToolsPath [ "gen-hydra-key"
                         , "--output-file"
                         , [i|#{path}.hydra|]
                         ])
    ""
  when (not . null $ output) $ logInfo $ pretty output
  case exitCode of
    ExitSuccess -> do
      pure $ Right $ mkKeyPair [i|#{path}.hydra.sk|] [i|#{path}.hydra.vk|]
    _ -> pure $ Left stderr

-- | Publishes the reference scripts if they don't exist on chain, will read them otherwise
-- getReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => FilePath -> SigningKey -> m (Either String HydraScriptTxId)
-- getReferenceScripts scriptPath sk = runExceptT $ do
--   exists <- liftIO $ doesFileExist scriptPath
--   case exists of
--     True -> liftIO $ T.readFile scriptPath
--     False -> do
--       scripts <- ExceptT $ publishReferenceScripts sk
--       lift $ liftIO $ T.writeFile scriptPath scripts
--       pure scripts
--
-- -- TODO: Make this generic over Cardano node socket path/network id?
-- publishReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => SigningKey -> m (Either String HydraScriptTxId)
-- publishReferenceScripts sk = do
publishReferenceScripts :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m)
  => CardanoNodeInfo
  -> SigningKey
  -> m (Either String HydraScriptTxId)
publishReferenceScripts cninf sk = do
  logInfo $ "Publishing reference scripts ('νInitial' & 'νCommit')..."
  (fmap . fmap) (T.strip . T.pack) $ processAdapter $ liftIO $ readCreateProcessWithExitCode cp ""
  where
    cp = proc hydraNodePath [ "publish-scripts"
                              , "--network-id"
                              , show . _testNetMagic . _nodeType $ cninf
                              , "--node-socket"
                              , _nodeSocket cninf
                              , "--cardano-signing-key"
                              , getSigningKeyFilePath sk
                              ]


waitForTxIn :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> TxIn -> m ()
waitForTxIn cninf txin = do
  logInfo $ "Waiting for utxo " <> pretty txin <> ".."
  liftIO waitFn
  where
    waitFn = do
      exists <- txInExists cninf txin
      threadDelay 10000
      unless exists waitFn


cardanoNodeArgs :: CardanoNodeInfo -> [String]
cardanoNodeArgs cninf = ["--testnet-magic", show . _testNetMagic . _nodeType $ cninf]

writeProtocolParameters :: CardanoNodeInfo ->  IO ()
writeProtocolParameters cninf = do
  void $ readCreateProcess ((proc cardanoCliPath ([ "query"
                                    , "protocol-parameters"
                                    , "--out-file"
                                    , _nodeLedgerProtocolParameters cninf
                                    ]
                       <> cardanoNodeArgs cninf))
            { env = Just [( "CARDANO_NODE_SOCKET_PATH" , _nodeSocket cninf)]
            }) ""

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

queryAddressUTXOs :: MonadIO m => CardanoNodeInfo -> Address -> m WholeUTXO
queryAddressUTXOs cninf addr = liftIO $ do
  let queryProc =
        (proc cardanoCliPath $ [ "query"
                             , "utxo"
                             , "--address"
                             , addressToString addr
                             , "--out-file"
                             , "/dev/stdout"
                             ]
            <> cardanoNodeArgs cninf)
        { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }
  (_, str, _) <- readCreateProcessWithExitCode queryProc ""
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

buildSignedTx :: CardanoNodeInfo -> SigningKey -> SomeTx -> IO (Either String (FilePath, TxId))
buildSignedTx nodeInfo signingKey tx = runExceptT $ do
  draftTx <- ExceptT $ buildDraftTx nodeInfo tx
  signedTx <- ExceptT $ signTx nodeInfo signingKey draftTx
  txid <- ExceptT $ txIdFromSignedTx signedTx
  pure (signedTx,txid)

-- | Convenience for getting faucet Output for seeding
getFirstTxIn :: CardanoNodeInfo -> Address -> IO (Either String TxIn)
getFirstTxIn cninf addr = runExceptT $ do
  output <- ExceptT $ processAdapter $ readCreateProcessWithExitCode cp ""
  a <- ExceptT $ processAdapter $ readProcessWithExitCode jqPath ["-r", "keys[0]"] output
  pure $ T.strip $ T.pack a
  where
    cp = (proc cardanoCliPath $
                              [ "query"
                              , "utxo"
                              , "--address"
                              , addressToString addr
                              , "--out-file"
                              , "/dev/stdout"
                              ]
                              <> cardanoNodeArgs cninf
        ) { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }

getCardanoAddress :: CardanoNodeInfo -> VerificationKey -> IO (Either String Address)
getCardanoAddress cninf keyPath = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> pure $ Right $ UnsafeToAddress . T.pack $ stdout
    _ -> pure $ Left stderr
  where
    cp = (proc cardanoCliPath $ [ "address"
                              , "build"
                              , "--payment-verification-key-file"
                              , getVerificationKeyFilePath keyPath
                              ]
               <> cardanoNodeArgs cninf
         ) { env = Just [("CARDANO_NODE_SOCKET_PATH", _nodeSocket cninf)] }

-- | Returns an error if the address doesn't have enough funds to cover fees.
transferAll :: () => CardanoNodeInfo -> SigningKey -> Address -> Address -> IO (Either String TxIn)
transferAll cninf signingKey fromAddr destAddr = do
  txins <- getAllLovelaceUtxos cninf fromAddr
  transferAmount cninf signingKey (fmap toInteger txins) destAddr True Nothing
  

seedAddressFromFaucetAndWait :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => CardanoNodeInfo -> KeyPair -> Address -> Lovelace -> Bool -> m (Either String TxIn)
seedAddressFromFaucetAndWait cninf faucetKeys addr amount makeFuel = runExceptT $ do
  txin <- ExceptT $ liftIO $ seedAddressFromFaucet cninf faucetKeys addr amount makeFuel
  lift $ waitForTxIn cninf txin
  pure txin

-- | Send an amount in lovelace to the named actor
seedAddressFromFaucet :: CardanoNodeInfo -> KeyPair -> Address -> Lovelace -> Bool -> IO (Either String TxIn)
seedAddressFromFaucet cninf (KeyPair faucetsk faucetvk) addr amount makeFuel = runExceptT $ do
  draftTx <- ExceptT $ buildSeedTxForAddress cninf faucetvk addr amount makeFuel
  signedTx <- ExceptT $ signTx cninf faucetsk draftTx
  txin <- txInput 0 <$> (ExceptT $ txIdFromSignedTx signedTx)
  _ <- ExceptT $ submitTx cninf signedTx
  pure txin

buildDraftTx :: () => CardanoNodeInfo -> SomeTx -> IO (Either String FilePath)
buildDraftTx cninf tx = do
  filename <- getTempPath'
  (exitCode, _, stderr) <- readCreateProcessWithExitCode ((proc cardanoCliPath $ filter (/= "")
                                     [ "transaction"
                                     , "build"
                                     , "--babbage-era"
                                     , "--cardano-mode"
                                     , "--change-address"
                                     , addressToString (_tx_changeAddr tx)
                                     ]
                                     <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) $ _tx_ins tx)
                                     <>
                                     [ "--tx-out"
                                     , addressToString (_tx_outAddr tx) <> "+" <> show (_tx_outAmount tx)
                                     ]
                                     <> maybe [] (\h -> [ "--tx-out-datum-hash", T.unpack h ]) (_tx_outDatumHash tx)
                                     <>
                                     [ "--out-file"
                                     , filename
                                     ]
                                     <> cardanoNodeArgs cninf)
                                     { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
                                  ""
  case exitCode of
    ExitSuccess -> pure $ Right filename
    _ -> pure $ Left stderr


buildRawEmptyTx :: () => CardanoNodeInfo -> [TxIn] -> [Address] -> IO FilePath
buildRawEmptyTx cninf txins outAddrs = do
  filename <- getTempPath'
  _ <- readCreateProcess ((proc cardanoCliPath $ filter (/= "")
                              [ "transaction"
                              , "build-raw"
                              ]
                              <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) $ txins)
                              <>
                              concatMap (\addr -> [ "--tx-out", addressToString addr <> "+0" ]) outAddrs
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

-- | Successful only if there is enough funds available
transferAmount :: CardanoNodeInfo -> SigningKey -> Map TxIn Lovelace -> Address -> Bool -> Maybe Lovelace -> IO (Either String TxId)
transferAmount cninf signingKey utxos toAddr minusFee maybeAmount = do
  let txins = Map.keys utxos
  let fullAmount :: Lovelace = sum . Map.elems $ utxos
  emptyTx <- buildRawEmptyTx cninf txins [toAddr]
  fee <- calculateMinFees cninf emptyTx (Map.size utxos) 1
  let amount = fromMaybe fullAmount maybeAmount
  if (if minusFee
      then fee >= amount || amount > fullAmount
      else fee + amount >= fullAmount)
    then pure $ Left "Insufficient funds to transfer"
    else runExceptT $ do
      tipSlotNo <- lift $ getTipSlotNo cninf
      rawTx <- ExceptT $ buildRawTx cninf txins (Map.singleton toAddr (bool (+) (-) minusFee fullAmount fee)) (tipSlotNo + 200) fee
      signedTx <- ExceptT $ signTx cninf signingKey rawTx
      ExceptT $ submitTx cninf signedTx



buildRawTx :: CardanoNodeInfo -> [TxIn] -> Map Address Lovelace -> Integer -> Lovelace -> IO (Either String FilePath)
buildRawTx cninf txins outAmounts invalidAfter fee = do
  outFile <- getTempPath'
  (exitCode, _, stderr) <- readCreateProcessWithExitCode
    ((proc cardanoCliPath $ ["transaction", "build-raw"
                            ]
                            <> concatMap (\txin -> ["--tx-in", T.unpack txin]) txins
                            <> concatMap (\(addr,amount) -> [ "--tx-out", [i|#{addressToString addr}+#{amount}|] ]) (Map.toList outAmounts)
                            <> [ "--invalid-hereafter", show invalidAfter
                               , "--fee", show fee
                               , "--out-file", outFile
                               ]
     )
     { env = Just [("CARDANO_NODE_SOCKET_PATH",  _nodeSocket cninf)] })
    ""
  case exitCode of
    ExitSuccess -> pure $ Right outFile
    _ -> pure $ Left stderr


buildSeedTxForAddress :: CardanoNodeInfo -> VerificationKey -> Address -> Lovelace -> Bool -> IO (Either String DraftTx)
buildSeedTxForAddress cninf faucetvk addr amount makeFuel = runExceptT $ do
  faucetAddr <- ExceptT $ getCardanoAddress cninf faucetvk
  hash <- ExceptT $ getFirstTxIn cninf faucetAddr
  ExceptT $ buildDraftTx cninf $ SomeTx
      { _tx_ins = [hash],
        _tx_outAddr = addr,
        _tx_outAmount = amount,
        _tx_changeAddr = faucetAddr,
        _tx_outDatumHash = fuelMarkerDatumHash <$ guard makeFuel
      }

-- | Sign a transaction and a path containing it.
signTx :: CardanoNodeInfo -> SigningKey -> DraftTx -> IO (Either String SignedTx)
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
  (exitCode, _, stderr) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> pure $ Right outFile
    _ -> pure $ Left stderr

txIdFromSignedTx :: SignedTx -> IO (Either String TxId)
txIdFromSignedTx filename = do
  (exitCode, output, stderr) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> pure $ Right $ T.strip . T.pack $ output
    _ -> pure $ Left stderr
  where
    cp =
      ( proc cardanoCliPath $
          [ "transaction",
            "txid",
            "--tx-file",
            filename
          ]
      )

-- | Try to submit a Tx on the given Node, returns either what went wrong or the TxId submitted
submitTx :: CardanoNodeInfo -> SignedTx -> IO (Either String TxId)
submitTx cninf signedFile = runExceptT $ do
  txid <- ExceptT $ txIdFromSignedTx signedFile
  _ <- ExceptT $ processAdapter $ readCreateProcessWithExitCode cp ""
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
buildSignedHydraTx :: SigningKey -> Address -> Address -> Map TxIn Lovelace -> Lovelace -> IO (Either String (T.Text, String))
buildSignedHydraTx signingKey fromAddr toAddr txInAmounts amount = runExceptT $ do
  let fullAmount = sum txInAmounts
  txBodyPath <- lift $ snd <$> getTempPath
  _ <- ExceptT $ processAdapter $ readCreateProcessWithExitCode (proc cardanoCliPath
                       ([ "transaction"
                        , "build-raw"
                        , "--babbage-era"
                        ]
                        <> (concatMap (\txin -> ["--tx-in", T.unpack txin]) . Map.keys $ txInAmounts)
                        <>
                        [ "--tx-out"
                        , [i|#{addressToString toAddr}+#{amount}|]
                        , "--tx-out"
                        , [i|#{addressToString fromAddr}+#{fullAmount - amount}|]
                        , "--fee"
                        , "0"
                        , "--out-file"
                        , txBodyPath
                        ]))
    ""
  txid <- ExceptT $ processAdapter $ readCreateProcessWithExitCode
    (proc cardanoCliPath
      [ "transaction"
      , "txid"
      , "--tx-body-file"
      , txBodyPath
      ])
    ""
  fmap (T.strip . T.pack $ txid,) $ ExceptT $ processAdapter $ readCreateProcessWithExitCode
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
  pure $ for is (eitherToMaybe . parseAddress <=< flip lookup addrs)

getDevnetAddress :: Int -> IO (Maybe Address)
getDevnetAddress addrIndex = do
  addrs <- getDevnetAddresses [addrIndex]
  pure $ join $ headMay <$> addrs
