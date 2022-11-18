{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Backend

(backend)

where

import Prelude hiding (filter)

import Hydra.Types
import Hydra.Devnet

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Common.Helpers
import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core

import Control.Monad.Log
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Foldable

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
import HydraPay.Client

import CardanoNodeInfo

import HydraPay.Api
import Control.Monad ((<=<), forever, when, guard)

import Network.WebSockets.Snap
import qualified Network.WebSockets as WS
import Data.Traversable (forM)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe
import System.Directory (doesFileExist)
import qualified HydraPay.WebSocketDemo as WSD

setupDemo :: State -> IO ([(KeyPair, Address)])
setupDemo state = do
  cns <- readMVar (_state_cardanoNodeState state)
  case cardanoNodeState_nodeType cns of
    CfgDevnet -> do
      prefix <- liftIO getTempPath'
      oneKs <- withLogging $ generateCardanoKeys $ prefix <> "one"
      one <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey oneKs
      twoKs <- withLogging $ generateCardanoKeys $ prefix <> "two"
      two <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey twoKs
      withLogging $ do
        seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys one (ada 10000) False
        seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys two (ada 10000) False
      -- seedTestAddresses (_hydraCardanoNodeInfo hydraSharedInfo) devnetFaucetKeys 10
      pure [(oneKs, one), (twoKs, two)]

    CfgPreview pcfg -> do
      forM (_previewParticipants pcfg) $ \kp -> do
        addr <- liftIO $ getCardanoAddress (_previewNodeInfo pcfg) (_verificationKey kp)
        pure (kp, addr)

runLogging = flip runLoggingT (print . renderWithSeverity id)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- NOTE(skylar): Running heads is a map from head name to network handle
      flip runLoggingT (print . renderWithSeverity id) $ do
        liftIO $ runHydraPay $ \state -> do
          hsi <- getHydraSharedInfo state
          participants <- setupDemo state
          let
            cninf = _hydraCardanoNodeInfo hsi
            apiKey = _state_apiKey state
          liftIO . serve $ \case
            BackendRoute_HydraPay :/ hpr -> case hpr of
              HydraPayRoute_Api :/ () -> websocketApiHandler state

            BackendRoute_DemoApi :/ () -> do
              runWebSocketsSnap $ \pendingConn -> do
                conn <- WS.acceptRequest pendingConn
                WS.forkPingThread conn 30
                forever $ do
                  mClientMsg <- Aeson.decode <$> WS.receiveData conn
                  case mClientMsg of
                    Just clientMsg -> WSD.handleClientMessage apiKey state cninf participants clientMsg >>= WS.sendTextData conn . Aeson.encode
                    Nothing -> WS.sendTextData conn . Aeson.encode $ InvalidMessage
              pure ()

            BackendRoute_Api :/ () -> pure ()
            BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
