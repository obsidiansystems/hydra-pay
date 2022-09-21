{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Reflex.Dom.WebSocket
import Reflex.Dom.Prerender
import Common.Api
import Common.Route
import Control.Monad.IO.Class (liftIO)


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ (pure ()) frontend'
  }

frontend' :: forall t m. (DomBuilder t m,
                   PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, _) => m ()
frontend' = mdo
  text "Hello"
  let wsCfg :: (WebSocketConfig t T.Text) = WebSocketConfig toSend never False []
  ws <- textWebSocket "ws://localhost:9001" wsCfg
  performEvent_ (liftIO . putStrLn . T.unpack <$> _webSocket_recv ws)
  performEvent_ (liftIO . print <$> toSend)
  let toSend = ["{ \"tag\": \"Init\", \"contestationPeriod\": 60 }" ] <$ _webSocket_open ws
  return ()
