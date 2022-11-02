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

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core

import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.String.Interpolate ( i )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc

import qualified System.IO.Streams as Streams

import Control.Concurrent
import System.Process

import Data.Aeson as Aeson
    ( decode, encode, FromJSON, ToJSON )

import qualified Data.ByteString.Lazy as LBS

import HydraPay
import CardanoNodeInfo
import Control.Monad ((<=<))

getDevnetHydraSharedInfo :: (MonadIO m, MonadLog (WithSeverity (Doc ann)) m) => m HydraSharedInfo
getDevnetHydraSharedInfo = do
  scripts <- getReferenceScripts "devnet/scripts" (_signingKey devnetFaucetKeys)
  pure $ HydraSharedInfo
    { _hydraScriptsTxId = T.unpack scripts,
      _ledgerGenesis = "devnet/genesis-shelley.json",
      _ledgerProtocolParameters = "devnet/protocol-parameters.json",
      _cardanoNodeInfo = cardanoDevnetNodeInfo
    }

cardanoDevnetNodeInfo :: CardanoNodeInfo
cardanoDevnetNodeInfo = CardanoNodeInfo (TestNet 42) "devnet/node.socket"

devnetFaucetKeys :: KeyPair
devnetFaucetKeys = mkKeyPair "devnet/credentials/faucet.sk" "devnet/credentials/faucet.vk"

-- TODO: See if it's okay to change Either a (Maybe b) to Just Either a b.
-- What does writing toJSON () to response do?
handleJsonRequestBody :: (MonadSnap m, ToJSON a,
                          FromJSON t, ToJSON b) =>
  (t -> LoggingT (WithSeverity (Doc ann)) IO (Either a (Maybe b))) ->
  m ()
handleJsonRequestBody f = do
  runRequestBody Streams.toList
    >>= ( \case
            Nothing -> do
              modifyResponse $ setResponseStatus 400 "Bad Request"
              writeLBS $ Aeson.encode InvalidPayload
            Just x -> do
              result <- liftIO $ withLogging $ f x
              case result of
                Right a -> mapM_ (writeLBS . Aeson.encode) a
                Left err -> writeLBS $ Aeson.encode err
        )
      . Aeson.decode
      . LBS.fromChunks

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
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
                  handleJsonRequestBody (fmap ((Nothing :: Maybe ()) <$) . initHead state)
                HydraPayRoute_Commit :/ () -> do
                  handleJsonRequestBody (fmap ((Nothing :: Maybe ()) <$) . commitToHead state)
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

                HydraPayRoute_Close :/ name -> do
                  status <- closeHead state name
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

                HydraPayRoute_SubmitTx :/ addr -> do
                  handleJsonRequestBody (fmap ((Nothing :: Maybe ()) <$) . submitTxOnHead state addr)

                HydraPayRoute_Head :/ () -> do
                  handleJsonRequestBody $
                    fmap (Just <$>)
                    . mapM (getHeadStatus state . _head_name)
                    <=< createHead state

                HydraPayRoute_HeadBalance :/ addr -> do
                  handleJsonRequestBody (fmap (Just <$>) . headBalance state addr)
                HydraPayRoute_L1Balance :/ addr -> do
                  writeLBS . Aeson.encode =<< l1Balance state addr
              BackendRoute_DemoAddresses :/ () -> do
                addrs <- liftIO $ T.lines <$> T.readFile "devnet/addresses"
                writeLBS $ Aeson.encode addrs
                pure ()
              BackendRoute_Api :/ () -> pure ()
              BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }

seconds :: Int -> Int
seconds = (* 1000000)
