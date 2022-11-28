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
import Control.Monad.Log
import Data.Text.Prettyprint.Doc (Doc)
import HydraPay
import Obelisk.Backend
import Obelisk.Route
import Prelude hiding (filter)

runLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
runLogging = flip runLoggingT (print . renderWithSeverity id)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      runLogging $ do
        liftIO $ runHydraPay $ \state -> do
          liftIO . serve $ \case
            BackendRoute_HydraPay :/ hpr -> case hpr of
              HydraPayRoute_Api :/ () -> websocketApiHandler state
            BackendRoute_Api :/ () -> pure ()
            BackendRoute_Missing :/ _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
