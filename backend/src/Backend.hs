{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Backend

(backend)

where

import Prelude hiding (filter)

import Hydra.Devnet
import HydraPay.Client

import Common.Route
import Obelisk.Backend
import Obelisk.Route

import Snap.Core

import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.String.Interpolate ( i )
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import qualified System.IO.Streams as Streams

import Control.Concurrent
import System.Process

import Data.Aeson as Aeson
    ( decode, encode )



import qualified Data.ByteString.Lazy as LBS

import HydraPay
import CardanoNodeInfo

getDevnetHydraSharedInfo :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraSharedInfo
getDevnetHydraSharedInfo = do
  scripts <- getReferenceScripts (_verificationKey devnetFaucetKeys) "devnet/scripts"
  pure $ HydraSharedInfo
    { _hydraScriptsTxId = T.unpack scripts,
        _ledgerGenesis = "devnet/genesis-shelley.json",
        _ledgerProtocolParameters = "devnet/protocol-parameters.json",
        _cardanoNodeInfo = cardanoDevnetNodeInfo
      }


cardanoDevnetNodeInfo :: CardanoNodeInfo
cardanoDevnetNodeInfo = CardanoNodeInfo (TestNet 42) "devnet/node.socket"

devnetFaucetKeys :: KeyPair
devnetFaucetKeys = KeyPair { _signingKey = "devnet/credentials/faucet.sk"
                           , _verificationKey = "devnet/credentials/faucet.vk"
                           }

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do -- withHydraPool $ \pool -> do
      -- NOTE(skylar): Running heads is a map from head name to network handle
      flip runLoggingT (print . renderWithSeverity id) $ do
        prepareDevnet
        liftIO $ withCreateProcess cardanoNodeCreateProcess $ \_ _stdout _ _handle -> do
          flip runLoggingT (print . renderWithSeverity id) $ do
            logMessage $ WithSeverity Informational [i|
              Cardano node is running
              |]
            liftIO $ threadDelay $ seconds 3
            seedTestAddresses cardanoDevnetNodeInfo devnetFaucetKeys 10
            state <- getHydraPayState =<< getDevnetHydraSharedInfo
            logMessage $ WithSeverity Informational [i|
              Serving
              |]
            liftIO . serve $ \case
              BackendRoute_HydraPay :/ hpr -> case hpr of
                HydraPayRoute_Init :/ () -> do
                  runRequestBody Streams.toList >>= (\case
                    Nothing -> do
                      modifyResponse $ setResponseStatus 400 "Bad Request"
                      writeLBS $ Aeson.encode InvalidPayload

                    Just hi -> do
                      result <- liftIO $ withLogging $ initHead state hi
                      case result of
                        Right () -> pure ()
                        Left err -> writeLBS $ Aeson.encode err) . Aeson.decode . LBS.fromChunks

                HydraPayRoute_Commit :/ () -> do
                  runRequestBody Streams.toList >>= (\case
                    Nothing -> do
                      modifyResponse $ setResponseStatus 400 "Bad Request"
                      writeLBS $ Aeson.encode InvalidPayload

                    Just hc -> do
                      result <- liftIO $ withLogging $ commitToHead state hc
                      case result of
                        Right _head -> pure ()
                        Left err -> writeLBS $ Aeson.encode err) . Aeson.decode . LBS.fromChunks

                HydraPayRoute_AddFuelTx :/ (addr, amount) -> do
                  result <- buildAddTx Fuel state addr amount
                  case result of
                    Left err -> writeLBS $ Aeson.encode err
                    Right tx -> writeLBS $ Aeson.encode tx

                HydraPayRoute_AddFundsTx :/ (addr, amount) -> do
                  result <- buildAddTx Funds state addr amount
                  case result of
                    Left err -> writeLBS $ Aeson.encode err
                    Right tx -> writeLBS $ Aeson.encode tx

                HydraPayRoute_HeadStatus :/ name -> do
                  status <- getHeadStatus state name
                  writeLBS $ Aeson.encode status

                HydraPayRoute_Withdraw :/ () -> do
                  runRequestBody Streams.toList >>= (\case
                    Nothing -> do
                      modifyResponse $ setResponseStatus 400 "Bad Request"
                      writeLBS $ Aeson.encode InvalidPayload

                    Just wr -> do
                      result <- liftIO $ withLogging $ withdraw state wr
                      case result of
                        Right txid -> writeLBS $ Aeson.encode txid
                        Left err -> writeLBS $ Aeson.encode err) . Aeson.decode . LBS.fromChunks

                HydraPayRoute_Head :/ () -> do
                  runRequestBody Streams.toList >>= (\case
                    Nothing -> do
                      modifyResponse $ setResponseStatus 400 "Bad Request"
                      writeLBS $ Aeson.encode InvalidPayload

                    Just hc -> do
                      result <- liftIO $ withLogging $ createHead state hc
                      case result of
                        Right head' -> do
                          status <- getHeadStatus state (_head_name head')
                          writeLBS $ Aeson.encode status
                        Left err -> writeLBS $ Aeson.encode err) . Aeson.decode . LBS.fromChunks

              BackendRoute_Api :/ () -> pure ()
              BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }

seconds :: Int -> Int
seconds = (* 1000000)
