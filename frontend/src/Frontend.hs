{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
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

import Reflex.Dom.GadtApi

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      rec
        (_, requests) <- runRequesterT app responses
        responses <- performWebSocketRequests "ws://localhost:8000/api" requests
      pure ()
      -- prerender_ (pure ()) frontend'
  }
requestingJs
  :: ( Reflex t
     , MonadFix m
     , Prerender t m
     , Requester t m
     , Requester t (Client m)
     , Request m ~ DemoApi
     , Request (Client m) ~ DemoApi
     , Response (Client m) ~ Either T.Text
     )
  => Event t (Request (Client m) a)
  -> m (Event t (Response (Client m) a))
requestingJs r = fmap (switch . current) $ prerender (pure never) $ requesting r

watchDevnetUTXOs :: ( Prerender t m
                    , PostBuild t m
                    , MonadHold t m
                    , MonadFix m
                    , DomBuilder t m
                    , Requester t m
                    , Request m ~ DemoApi
                    , Response m ~ Either T.Text
                    , Requester t (Client m)
                    , Request (Client m) ~ DemoApi
                    , Response (Client m) ~ Either T.Text) => m (Dynamic t T.Text)
watchDevnetUTXOs = do
  tick <- fmap switchDyn $ prerender (pure never) $ tickLossyFromPostBuildTime 2
  result <- requestingJs $ DemoApi_GetWholeUTXO <$ tick
  holdDyn "Loading" $ fmapMaybe eitherToMaybe result

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

-- FIXME(parenthetical): copied from hydra-demo
-- data HeadState
--   = Idle
--   | Initializing {parties :: [Party], remainingParties :: [Party], utxo :: UTxO}
--   | Open {parties :: [Party], utxo :: UTxO}
--   | Closed {contestationDeadline :: UTCTime}
--   | FanoutPossible
--   | Final {utxo :: UTxO}
--   deriving (Eq, Show, Generic)


app :: forall t m. ( Prerender t m
       , PostBuild t m
       , MonadHold t m
       , MonadFix m
       , DomBuilder t m
       , Requester t m
       , Request m ~ DemoApi
       , Response m ~ Either T.Text
       , Requester t (Client m)
       , Request (Client m) ~ DemoApi
       , Response (Client m) ~ Either T.Text) => m ()
app = elClass "div" "w-screen h-screen bg-gray-900 overflow-hidden" $ do
  elClass "div" "p-4 m-4 text-white text-5xl font-bold" $ text "Hydra Proof Of Concept Demo"

  elClass "div" "p-4 m-4 rounded-lg bg-gray-800 text-white" $ do
    elClass "div" "text-xl p-2 font-bold" $ text "Current Devnet UTXOs"
    elClass "div" "p-2 border-white border-t" $ watchDevnetUTXOs >>= dynText
  prerender_ blank $ mdo
    forM_ [ ("alice", "ws://localhost:9001")
          , ("bob", "ws://localhost:9002")
          , ("carol", "ws://localhost:9003")
          ]
      $ \(name, wsUrl) ->
          elClass "div" "bg-gray-900 text-white" $ mdo
            let wsCfg :: (WebSocketConfig t T.Text) = WebSocketConfig toSend never False []
            ws <- textWebSocket wsUrl wsCfg
            el "h1" $ text name
            el "ul" $ do
              comms <- foldDyn (++) [] $
                (fmap ((:[]) . ("Rcv: " <>)) $ _webSocket_recv ws)
                <>
                fmap (fmap ("Snd: " <>)) toSend
              dyn_ $ fmap (mapM (el "li" . text) . reverse) comms
            toSendDyn <- fmap (:[]) . _inputElement_value <$> inputElement (
              def & inputElementConfig_setValue .~ ("" <$ toSend))
            sendClick <- button "send"
            let toSend =
                  leftmost [--  ["{ \"tag\": \"Init\", \"contestationPeriod\": 60 }"] <$ _webSocket_open ws
                           -- ,
                             current toSendDyn <@ sendClick
                           ]
            pure ()
    pure ()
