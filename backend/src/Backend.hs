module Backend where

import HydraPay
import HydraPay.PortRange
import HydraPay.Database
import HydraPay.Logging
import HydraPay.Cardano.Hydra
import HydraPay.Utils

import Common.Route

import qualified HydraPay.Database as Db

import Control.Exception
import Control.Lens
import Control.Monad
import Network.WebSockets as WS
import Network.WebSockets.Snap as WS
import Obelisk.Route
import Obelisk.Backend
import Reflex.Dom.GadtApi.WebSocket

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      runPreviewInstance
  , _backend_routeEncoder = fullRouteEncoder
  }
