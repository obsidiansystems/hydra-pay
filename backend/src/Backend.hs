{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Backend

(backend)

where

import Common.Route
import Control.Monad.IO.Class (liftIO)
import HydraPay
import HydraPay.Logging
import Obelisk.Backend
import Obelisk.Route
import Prelude hiding (filter)
import Config

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- Greet the user
      putStrLn hydraAsciiLogo
      putStrLn hydraPayAsciiLogo
      putStrLn hydraPayAsciiSubhead

      cfg <- getHydraCLIConfig
      withLogging $ do
        liftIO $ runHydraPay cfg $ \state -> do
          liftIO . serve $ \case
            BackendRoute_HydraPay :/ hpr -> case hpr of
              HydraPayRoute_Api :/ () -> websocketApiHandler state
            BackendRoute_Api :/ () -> pure ()
            BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
