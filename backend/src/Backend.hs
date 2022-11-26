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


import Common.Route
import Obelisk.Backend
import Obelisk.Route

import Control.Monad.Log
import Control.Monad.IO.Class (liftIO)

import Snap.Core

import Control.Concurrent

import Data.Aeson as Aeson
    ( decode, encode )


import HydraPay


import HydraPay.Api
import Control.Monad (forever)

import Network.WebSockets.Snap
import qualified Network.WebSockets as WS
import Data.Traversable (forM)
import qualified HydraPay.WebSocketDemo as WSD
import Data.Text.Prettyprint.Doc (Doc)

setupDemo :: State -> IO [(KeyPair, Address)]
setupDemo state = do
  cns <- readMVar (_state_cardanoNodeState state)
  hydraSharedInfo <- getHydraSharedInfo state
  case cardanoNodeState_nodeType cns of
    CfgDevnet -> do
      prefix <- liftIO getTempPath'
      oneKs <- withLogging $ generateCardanoKeys $ prefix <> "one"
      one <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey oneKs
      twoKs <- withLogging $ generateCardanoKeys $ prefix <> "two"
      two <- liftIO $ getCardanoAddress cardanoDevnetNodeInfo $ _verificationKey twoKs

      withLogging $ seedTestAddresses (_hydraCardanoNodeInfo hydraSharedInfo) devnetFaucetKeys 10
      --withLogging $ do
      --  seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys one (ada 10000) False
      --  seedAddressFromFaucetAndWait cardanoDevnetNodeInfo devnetFaucetKeys two (ada 10000) False
      pure [(oneKs, one), (twoKs, two)]

    CfgPreview pcfg -> do
      forM (_previewParticipants pcfg) $ \kp -> do
        addr <- liftIO $ getCardanoAddress (_previewNodeInfo pcfg) (_verificationKey kp)
        pure (kp, addr)

runLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
runLogging = flip runLoggingT (print . renderWithSeverity id)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- NOTE(skylar): Running heads is a map from head name to network handle
      runLogging $ do
        liftIO $ runHydraPay $ \state -> do
          hsi <- getHydraSharedInfo state
          -- participants <- setupDemo state
          let
            participants = []
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

            BackendRoute_DemoAddresses :/ () -> do
              writeText "100000000"

            BackendRoute_DemoTestWithdrawal :/ () -> do
              writeText "Done"

            BackendRoute_Api :/ () -> pure ()
            BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
