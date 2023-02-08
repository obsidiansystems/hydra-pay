{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend
  ( frontend
  )
where

import Prelude hiding (filter)

import Text.Printf (printf)
import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Int
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Encode.Pretty as Aeson

import Hydra.Types
import HydraPay.Api hiding (amount)

import Text.Read (readMaybe)

import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route

import Reflex.Dom.Core
import Common.Route
import Common.Helpers

import Control.Lens
import Control.Monad.Fix
import Control.Monad.Trans.Class

import Language.Javascript.JSaddle (MonadJSM, jsg, js, js1, js2, liftJSM, fromJSValUnchecked)
import Data.Bool (bool)
import HydraPay.Config (HydraPayMode (ConfiguredMode), CardanoNodeParams (..))
import Data.String.Interpolate ( iii, __i )
import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Obelisk.Route.Frontend

import Frontend.LiveDocs

default (T.Text)

withHydraPayConnection :: (DomBuilder t m, Prerender t m) => (HydraPayConnection t -> EventWriterT t [ClientMsg] (Client m) a) -> m ()
withHydraPayConnection action = do
  prerender_ (elClass "div" "w-full h-full flex items-center justify-center text-xl text-gray-700" $ text "Personalizing your Hydra Pay Instance") $ do
    pb <- getPostBuild
    endpoint <- liftJSM $ do
      let
        webSocketProtocol :: T.Text -> T.Text
        webSocketProtocol "https:" = "wss:"
        webSocketProtocol _ = "ws:"

      wsProtocol <- fmap webSocketProtocol $ fromJSValUnchecked =<< jsg "location" ^. js "protocol"

      theHostname <- fromJSValUnchecked =<< jsg "location" ^. js "hostname"
      thePort <- fromJSValUnchecked =<< jsg "location" ^. js "port"

      pure $ mconcat [ wsProtocol
                   , "//"
                   , theHostname
                   , ":"
                   , thePort
                   , "/hydra/api"
                   ]
    rec
      lastTagId <- foldDyn (+) 0 $ fromIntegral . length <$> sendToHydraPay

      authenticated <- holdDyn False authResponse
      let
        sendToHydraPay =
          attachWith (\tid msgs -> uncurry Tagged <$> zip [tid..] msgs) (current lastTagId) sendMsg

        serverMsg = fmapMaybe id $ rws ^. webSocket_recv

        subscriptions = fmapMaybe (preview _PlainMsg) serverMsg
        responses = fmapMaybe (preview _TaggedMsg) serverMsg

        authResponse = fmapMaybe (preview _AuthResult . tagged_payload) responses

        connection = HydraPayConnection authenticated lastTagId subscriptions responses

      rws :: RawWebSocket t (Maybe ApiMsg) <- jsonWebSocket endpoint $ def
        & webSocketConfig_send .~ sendToHydraPay

      (_, sendMsg :: Event t [ClientMsg]) <- runEventWriterT $ action connection
    pure ()

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Hydra Pay"

      elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com") blank
      elAttr "link" ("href"=:"https://fonts.googleapis.com/css2?family=Inria+Sans:wght@300&family=Inter:wght@100;200;300;400;500;600;700;800;900&family=Krona+One&family=Rajdhani:wght@300;400;500;600;700&display=swap" <> "rel"=:"stylesheet") blank
      elAttr "link" ("rel"=:"stylesheet" <> "href"=:"https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@48,400,0,0") blank
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank

      -- Highlight JS for syntax highlighting
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css") blank
      elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js") blank

  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen overflow-hidden flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $ do
      r <- askRoute
      withHydraPayConnection $ \conn -> do
        flip runRoutedT r $ app conn
  }

app :: (MonadJSM m, TriggerEvent t m, SetRoute t (R FrontendRoute) m, EventWriter t [ClientMsg] m, PostBuild t m, PerformEvent t m, MonadJSM (Performable m), DomBuilder t m, MonadHold t m, MonadFix m, Adjustable t m) => HydraPayConnection t -> RoutedT t (R FrontendRoute) m ()
app conn = do
  subRoute_ $ \case
    FrontendRoute_Login -> elClass "div" "text-gray-700 w-full h-full flex items-center justify-center" $ do
      elClass "div" "rounded-lg p-4 bg-white drop-shadow" $ do
        elClass "div" "text-3xl font-bold" $ text "🐲 Hydra Pay Live Preview"
        elClass "div" "mt-8 pl-px text-gray-600 font-semibold" $ text "Provisioned Key"
        showError <- elClass "div" " flex flex-row items-stretch" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "flex-grow border px-2" <> "placeholder" =: "Enter your provisioned key"
          (buttonEl, _) <- elClass' "button" "flex-grow-0 rounded-tr-md rounded-br-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Enter"
          let
            enteredKey = _inputElement_value ie

            writeKey key = do
              _ <- jsg "localStorage" ^. js2 "setItem" "apiKey" key
              pure ()

          (authFail, authSuccess) <- fmap fanBool $ auth conn $ current enteredKey <@ domEvent Click buttonEl

          performEvent_ $ liftJSM . writeKey <$> current enteredKey <@ authSuccess
          showError <- holdDyn False $ leftmost [True <$ authFail, False <$ _inputElement_input ie]
          setRoute $ FrontendRoute_App :/ () <$ authSuccess
          pure showError
        elClass "div" "pl-px mt-1 text-sm" $ elDynClass "div" (bool "opacity-0" "text-red-400 font-semibold" <$> showError) $ text "The key you provided is not valid"
        pure ()
    FrontendRoute_App -> do
      runLiveDocs conn
