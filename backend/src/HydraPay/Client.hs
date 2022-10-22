-- | This module contains integration testing and convenience functions for interacting with the hydra-pay service

module HydraPay.Client where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS

import Hydra.Devnet
import Hydra.Types
import HydraPay
import Network.HTTP.Simple

import System.IO
import System.IO.Temp
import System.Process

import qualified Data.Aeson as Aeson

getAndSubmitTx :: Address -> TxType -> IO ()
getAndSubmitTx addr tt = do
  let
    endpoint = case tt of
      Fuel -> "add-fuel"
      Funds -> "add-funds"

  req <- parseRequest $ "http://localhost:8000/hydra/" <> endpoint <> "/" <> T.unpack addr
  resp <- getResponseBody <$> httpJSON req
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
