module HydraPay.SubmitTxDirectTest where

import HydraPay
import CardanoNodeInfo
import Hydra.Devnet
import System.Process
import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO)

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import Control.Concurrent
import HydraPay.Client
import Control.Exception (finally)

import HydraPay.Api
import Hydra.Types
import Data.Either
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import System.IO
import System.IO.Temp

seconds :: Int -> Int
seconds = (* 1000000)


runLog :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
runLog = flip runLoggingT (print . renderWithSeverity id)

cardanoDevnetNodeInfo :: CardanoNodeInfo
cardanoDevnetNodeInfo = CardanoNodeInfo (TestNet 42) "devnet/node.socket" "devnet/protocol-parameters.json" "devnet/genesis-shelley.json"

devnetFaucetKeys :: KeyPair
devnetFaucetKeys = mkKeyPair "devnet/credentials/faucet.sk" "devnet/credentials/faucet.vk"

getDevnetHydraSharedInfo :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraSharedInfo
getDevnetHydraSharedInfo = do
  scripts <- getReferenceScripts "devnet/scripts" (_signingKey devnetFaucetKeys)
  pure $ HydraSharedInfo
    { _hydraScriptsTxId = T.unpack scripts,
        _cardanoNodeInfo = cardanoDevnetNodeInfo
      }

previewScriptsId = "4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d"

previewNodeInfo =
  CardanoNodeInfo
    { _nodeType = TestNet 2,
      _nodeSocket = "/tmp/cardano-node.socket",
      _nodeLedgerGenesis = "/etc/nixos/cardano-preview-testnet-config/shelley-genesis.json",
      _nodeLedgerProtocolParameters = "preview-parameters.json"
    }

previewHydraSharedInfo =
  HydraSharedInfo 
  { _hydraScriptsTxId = previewScriptsId,
    _cardanoNodeInfo = previewNodeInfo
  }


signAndSubmitTx' cninf (KeyPair sk _) tx = do
  withTempFile "." "tx.draft" $ \draftFile draftHandle -> do
    LBS.hPut draftHandle $ Aeson.encode tx
    hClose draftHandle

    withTempFile "." "tx.signed" $ \signedFile signedHandle -> do
      hClose signedHandle
      let cp = (proc cardanoCliPath [ "transaction"
                                    , "sign"
                                    , "--tx-body-file"
                                    , draftFile
                                    , "--signing-key-file"
                                    , getSigningKeyFilePath sk
                                    , "--out-file"
                                    , signedFile
                                    ])
            { env = Just [( "CARDANO_NODE_SOCKET_PATH" , _nodeSocket cninf)]
            }
      _ <- readCreateProcess cp ""
      submitTx cninf signedFile >>= withLogging . waitForTxIn cninf


runWDevnet :: (CardanoNodeInfo -> HydraSharedInfo -> KeyPair -> IO a) -> IO a
runWDevnet f = do 
  _ <- readCreateProcess (shell "rm -rf devnet") ""
  _ <- readCreateProcess (shell "rm -rf demo-logs") ""
  runLog $ prepareDevnet
  createProcess cardanoNodeCreateProcess
  threadDelay $ seconds 3
  shinfo <- runLog getDevnetHydraSharedInfo
  f cardanoDevnetNodeInfo shinfo devnetFaucetKeys

runWPreview :: (CardanoNodeInfo -> HydraSharedInfo -> KeyPair -> IO a) -> IO a
runWPreview f = do 
  f previewNodeInfo previewHydraSharedInfo (mkKeyPair "test-keys/a.cardano.sk" "test-keys/a.cardano.vk")



-- (state, ((one,oneKs), (two,twoKs))) <- runWDevnet testFn
testFn :: CardanoNodeInfo -> HydraSharedInfo -> KeyPair -> IO (State, ((Address, KeyPair), (Address, KeyPair)))
testFn cninf hydraSharedInfo faucetKeys = do
  oneKs <- runLog $ generateCardanoKeys "tmp/one"
  print oneKs
  one <- getCardanoAddress cninf $ _verificationKey oneKs
  twoKs <- runLog $ generateCardanoKeys "tmp/two"
  print twoKs
  two <- getCardanoAddress cninf $ _verificationKey twoKs

  let nodz = [one,two]

  state <- getHydraPayState hydraSharedInfo

  putStrLn "Seeding..."
  runLog $ seedAddressFromFaucetAndWait cninf faucetKeys one (ada 200) False
  runLog $ seedAddressFromFaucetAndWait cninf faucetKeys two (ada 200) False

  flip finally (do
                   putStrLn "CLEANING UP"
                   terminateHead state "a"
               ) $ do
    putStrLn "Creating head"
    _ <- createHead state (HeadCreate "a" nodz)

    putStrLn "Doing setup txs"
    let doTx typ addr keys adaAmount= do
          print "buildAddTx"
          (Right x) :: Either HydraPayError Tx <- buildAddTx typ state addr (ada adaAmount)
          print "Signing"
          signAndSubmitTx' cninf keys $ x
    doTx Funds one oneKs 70
    doTx Funds two twoKs 50
    doTx Fuel one oneKs 100
    doTx Fuel two twoKs 100

    putStrLn "Initing head"
    initHead state (HeadInit "a" one 5)
    
    threadDelay $ seconds 5

    putStrLn "Committing to nodes"
    threadDelay $ seconds 5
    commitToHead state $ HeadCommit "a" one (ada 70)
    threadDelay $ seconds 5
    commitToHead state $ HeadCommit "a" two (ada 50)
    
    -- threadDelay $ seconds 3
    -- -- putStrLn "NodeUTXOS one"
    -- -- print =<< withNode state "a" one (\x y -> fmap Right . getNodeUtxos x $ y)
    -- -- threadDelay $ seconds 3
    -- -- print =<< withNode state "a" two (\x y -> fmap Right . getNodeUtxos x $ y)
    -- putStrLn ""
    -- threadDelay $ seconds 10
    -- -- submitTxOnHead state one (HeadSubmitTx "a" two (ada 20))
    -- -- threadDelay $ seconds 1
    -- -- putStrLn "NodeUTXOS one"
    -- -- oneFunds <- headBalance state "a" one
    -- -- print oneFunds
    -- -- threadDelay $ seconds 1
    -- -- putStrLn "NodeUTXOS two"
    -- -- twoFunds <- headBalance state "a" two
    -- -- print twoFunds

    waitForHeadStatus state "a" Status_Open
    threadDelay $ seconds 2
    closeHead state "a"
    waitForHeadStatus state "a" Status_Finalized 

    threadDelay $ seconds 2

    -- print "Trying withdraw"
    -- -- TODO: How do I do this without risking InsufficientFunds?
    oneFnds <- getProxyFunds state one
    print oneFnds
    print =<< withdraw state (WithdrawRequest one (oneFnds - (ada 3)))
    twoFnds <- getProxyFunds state two
    print twoFnds
    print =<< withdraw state (WithdrawRequest two (twoFnds - (ada 3)))
    -- print =<< withdraw state (WithdrawRequest two (ada ))

    -- print ""
    -- print =<< getProxyFunds state one
    -- print ""
    -- print =<< getProxyFunds state two

    pure ()
  pure (state, ((one, oneKs), (two, twoKs)))
