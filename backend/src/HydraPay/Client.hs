-- | This module contains integration testing and convenience functions for interacting with the hydra-pay service

module HydraPay.Client where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Control.Monad

import Data.List (intercalate)
import Hydra.Devnet
import Hydra.Types
import HydraPay.Api
import HydraPay
import Network.HTTP.Client
import Network.HTTP.Simple

import System.IO
import System.IO.Temp
import System.Process

import Control.Concurrent

import Data.Foldable
import Data.Traversable

import qualified Data.Aeson as Aeson
import CardanoNodeInfo
import HydraPay.Api

testFanout :: CardanoNodeInfo -> T.Text -> IO ()
testFanout cninfo name = do
  testHeadParticipants cninfo name [1..3] $ fromIntegral contestation
  postCloseHead name
  threadDelay $ contestation * 1000000
  waitForFanout 1000000
  where
    contestation = 3
    waitForFanout falloff = do
      threadDelay falloff
      status <- fmap headStatus_status <$> getStatus name
      case status of
        Right Status_Finalized -> putStrLn "Finalization complete!"
        Right _ -> waitForFanout $ 2 * falloff
        Left err -> putStrLn $ "Failed: " <> err

testClose :: CardanoNodeInfo -> T.Text -> IO ()
testClose cninfo name = do
  testHeadParticipants cninfo name [1..3] 10
  postCloseHead name

testSendAndWithdraw :: CardanoNodeInfo -> Int -> IO ()
testSendAndWithdraw cninfo amount = do
  getAndSubmitTx cninfo 1 Funds 10000000
  threadDelay 1000000
  postWithdrawal 1 5000000

defaultContestation :: Int
defaultContestation = 60

testHeadOneParticipant :: CardanoNodeInfo -> T.Text -> IO ()
testHeadOneParticipant cninfo name = do
  postCreateHead name [1]
  threadDelay 1000000
  getAndSubmitTx cninfo 1 Funds funds
  threadDelay 1000000
  getAndSubmitTx cninfo 1 Fuel 1000000000
  threadDelay 1000000
  postInitHead name 1
  threadDelay 1000000
  postCommitHead name 1 funds
  threadDelay 1000000
  getStatus_ name
  where
    funds = 1000000000

testHeadParticipants :: CardanoNodeInfo -> T.Text -> [Int] -> Int -> IO ()
testHeadParticipants cninfo name partcipants@(initializer:_) con = do
  postCreateHead name partcipants
  threadDelay 1000000
  for_  partcipants $ \i -> do
    getAndSubmitTx cninfo i Funds funds
    getAndSubmitTx cninfo i Fuel 1000000000
    threadDelay 500000
  threadDelay 1000000
  postInitHeadCustomContestation con name initializer
  threadDelay 1000000
  for_  partcipants $ \i -> do
    postCommitHead name i funds
    threadDelay 1000000
  getStatus_ name
  where
    funds = 1000000000

postWithdrawalFullBalance :: Int -> IO ()
postWithdrawalFullBalance i = do
  Just addr <- getDevnetAddress i
  req <- parseRequest $ "http://localhost:8000/hydra/funds/" <> T.unpack addr
  balance :: Lovelace <- getResponseBody <$> httpJSON req
  postWithdrawal i balance

postWithdrawal :: Int -> Lovelace -> IO ()
postWithdrawal i amount = do
  Just addr <- getDevnetAddress i
  initReq <-  parseRequest $ "http://localhost:8000/hydra/withdraw"
  let
    payload = Aeson.encode $ WithdrawRequest addr (Just amount)
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postCreateHead :: T.Text -> [Int] -> IO ()
postCreateHead name indices = do
  Just addresses <- getDevnetAddresses indices
  initReq <- parseRequest $ "http://localhost:8000/hydra/heads"
  let
    payload = Aeson.encode $ HeadCreate name addresses
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postCloseHead :: T.Text -> IO ()
postCloseHead name = do
  putStrLn "Closing head"
  req <- parseRequest $ "http://localhost:8000/hydra/close/" <> T.unpack name
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postInitHead :: T.Text -> Int -> IO ()
postInitHead = postInitHeadCustomContestation defaultContestation

postInitHeadCustomContestation :: Int -> T.Text -> Int -> IO ()
postInitHeadCustomContestation contestation name i = do
  Just [addr] <- getDevnetAddresses [i]
  initReq <- parseRequest $ "http://localhost:8000/hydra/init"
  let
    payload = Aeson.encode $ HeadInit name addr (toInteger contestation)
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postCommitHead :: T.Text -> Int -> Lovelace -> IO ()
postCommitHead name i amount = do
  Just [addr] <- getDevnetAddresses [i]
  initReq <- parseRequest $ "http://localhost:8000/hydra/commit"
  let
    payload = Aeson.encode $ HeadCommit name addr amount
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

getStatus :: T.Text -> IO (Either String HeadStatus)
getStatus name = do
  req <- parseRequest $ "http://localhost:8000/hydra/head/" <> T.unpack name
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x
  let
    val :: Either String (Either HydraPayError HeadStatus) = Aeson.eitherDecode x
    ret = join $ fmap (mapLeft show) val
  pure ret

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft f (Right b) = Right b

getStatus_ :: T.Text -> IO ()
getStatus_ = void . getStatus

-- TODO: delete this function in favour of keys-as-values
getAndSubmitTx :: CardanoNodeInfo -> Int -> TxType -> Lovelace -> IO ()
getAndSubmitTx cninfo i tt amount = do
  Just [addr] <- getDevnetAddresses [i]
  resp <- getAddFundsTx addr amount
  case resp of
    Nothing -> putStrLn "Failed to get Tx"
    Just tx -> HydraPay.Client.signAndSubmitTx cninfo addr tx

-- TODO: delete this function in favour of keys-as-values
signAndSubmitTx :: CardanoNodeInfo -> Address -> Tx -> IO ()
signAndSubmitTx cninfo addr tx = do
  withTempFile "." "tx.draft" $ \draftFile draftHandle -> do
    LBS.hPut draftHandle $ Aeson.encode tx
    hClose draftHandle

    mKeypair <- getTestAddressKeys addr
    case mKeypair of
      Nothing -> error "Not a valid test address, check devnet/addresses"
      Just (KeyPair sk _) -> do
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
                { env = Just [( "CARDANO_NODE_SOCKET_PATH" , _nodeSocket cninfo)]
                }
          _ <- readCreateProcess cp ""
          submitTx cninfo signedFile >>= withLogging . waitForTxIn cninfo
