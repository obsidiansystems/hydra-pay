-- | This module contains integration testing and convenience functions for interacting with the hydra-pay service

module HydraPay.Client where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS

import Hydra.Devnet
import Hydra.Types
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

testHeadOneParticipant :: T.Text -> IO ()
testHeadOneParticipant name = do
  postCreateHead name [1]
  threadDelay 1000000
  getAndSubmitTx 1 Funds
  threadDelay 1000000
  getAndSubmitTx 1 Fuel
  threadDelay 1000000
  postInitHead name 1
  threadDelay 1000000
  postCommitHead name 1
  threadDelay 1000000
  getStatus name

testHeadParticipants :: T.Text -> [Int] -> IO ()
testHeadParticipants name partcipants@(initializer:_) = do
  postCreateHead name partcipants
  threadDelay 1000000
  for_  partcipants $ \i -> do
    getAndSubmitTx i Funds
    getAndSubmitTx i Fuel
    threadDelay 500000
  threadDelay 1000000
  postInitHead name initializer
  threadDelay 1000000
  for_  partcipants $ \i -> do
    postCommitHead name i
    threadDelay 1000000
  getStatus name

testHeadParticipantsNoDelays :: T.Text -> [Int] -> IO ()
testHeadParticipantsNoDelays name partcipants@(initializer:_) = do
  postCreateHead name partcipants
  for_  partcipants $ \i -> do
    getAndSubmitTx i Funds
    getAndSubmitTx i Fuel
  postInitHead name initializer
  for_  partcipants $ \i -> do
    postCommitHead name i
  getStatus name

getDevnetAddresses :: [Int] -> IO (Maybe [Address])
getDevnetAddresses is = do
  addrs <- zip [1..] . T.lines . T.pack <$> readFile addressesPath
  pure $ for is (flip lookup addrs)

postCreateHead :: T.Text -> [Int] -> IO ()
postCreateHead name indices = do
  Just addresses <- getDevnetAddresses indices
  initReq <- parseRequest $ "http://localhost:8000/hydra/heads"
  let
    payload = Aeson.encode $ HeadCreate name addresses True
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postInitHead :: T.Text -> Int -> IO ()
postInitHead name i = do
  Just [addr] <- getDevnetAddresses [i]
  initReq <- parseRequest $ "http://localhost:8000/hydra/init"
  let
    payload = Aeson.encode $ HeadInit name addr
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

postCommitHead :: T.Text -> Int -> IO ()
postCommitHead name i = do
  Just [addr] <- getDevnetAddresses [i]
  initReq <- parseRequest $ "http://localhost:8000/hydra/commit"
  let
    payload = Aeson.encode $ HeadCommit name addr
    req = setRequestBodyLBS payload $ initReq
      { method = "POST"
      }
  LBS.putStrLn $ "Request: " <> payload
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

getStatus :: T.Text -> IO ()
getStatus name = do
  req <- parseRequest $ "http://localhost:8000/hydra/head/" <> T.unpack name
  x <- getResponseBody <$> httpLBS req
  LBS.putStrLn . ("Response: " <>) $ x

getAndSubmitTx :: Int -> TxType -> IO ()
getAndSubmitTx i tt = do
  Just [addr] <- getDevnetAddresses [i]
  let
    endpoint = case tt of
      Fuel -> "add-fuel"
      Funds -> "add-funds"

  req <- parseRequest $ "http://localhost:8000/hydra/" <> endpoint <> "/" <> T.unpack addr
  resp <- getResponseBody <$> httpJSON req
  LBS.putStrLn $ "Received Tx: " <> Aeson.encode resp
  signAndSubmitTx addr resp
  pure ()

signAndSubmitTx :: Address -> Tx -> IO ()
signAndSubmitTx addr tx = do
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
                                        , sk
                                        , "--out-file"
                                        , signedFile
                                        ])
                { env = Just [("CARDANO_NODE_SOCKET_PATH", "devnet/node.socket")] }
          _ <- readCreateProcess cp ""
          submitTx signedFile >>= withLogging . waitForTxIn
