{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend
  ( frontend
  )
where

import Prelude hiding (filter)
import qualified Prelude as Pre

import Data.Int
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Hydra.Types
import HydraPay.Api

import Text.Read (readMaybe)
import Text.Printf (printf)

import System.Random
import Data.Map as Map
import qualified Data.Map as Map
import Data.Fixed
import Data.Bool (bool)
import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Common.Route

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Applicative
import Common.DemoApi
import Control.Monad.Trans.Class
import Language.Javascript.JSaddle (MonadJSM)

import Language.Javascript.JSaddle (jsg, js, liftJSM, fromJSValUnchecked)

default (T.Text)

balanceWidget :: (MonadHold t m, Prerender t m, PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t (Maybe Address) -> Event t a -> T.Text -> m (Dynamic t Float)
balanceWidget name addr refetch endpoint = do
  elClass "div" "text-lg flex flex-col mr-10" $ do
    elClass "div" "font-semibold" $ text name
    -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
    balanceDynChanged <- (fmap . fmap) join $ elClass "div" "font-semibold" $ dyn $ ffor addr $ \case
      Nothing -> pure $ constDyn 0
      Just addr -> prerender (pure $ constDyn 0) $ mdo
        addrLoad <- getPostBuild
        balanceResult :: Event t (Maybe Integer) <- getAndDecode $ "/" <> endpoint <> "/" <> addr <$ leftmost [addrLoad, delayedFail, () <$ refetch]
        let
          gotBalance = fmapMaybe (preview _Just) balanceResult
          reqFailed = fmapMaybe (preview _Nothing) balanceResult

        delayedFail <- delay 1 reqFailed
        mBalance <- holdDyn (Nothing :: Maybe Float) $ Just . lovelaceToAda <$> gotBalance

        dyn_ $ ffor mBalance $ \case
          Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
          Just balance -> do
            text $ T.pack . printf "%.2f" $ balance
            text " ADA"

        pure $ maybe 0 id <$> mBalance

    balanceDyn <- join <$> holdDyn (pure 0) balanceDynChanged
    pure $ balanceDyn

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
      -- elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank

      -- Highlight JS for syntax highlighting
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css") blank
      elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js") blank

  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen overflow-y-scroll overflow-x-hidden flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $ do
      elClass "div" "w-full h-full text-gray-700 max-w-4xl mx-auto p-4 rounded flex flex-col" $
        monitorView
  }

data DemoTx = DemoTx
  { demoTx_to :: T.Text
  , demoTx_amount :: Lovelace
  , demoTx_time :: Pico
  }
  deriving (Eq, Show)

showAsMs :: Pico -> T.Text
showAsMs = T.pack . printf "%.2f" . (realToFrac :: Pico -> Float) . (*1000)

header :: DomBuilder t m => T.Text -> m ()
header name = do
  elClass "div" "text-xl mt-8 mb-2 font-semibold" $ text name
  elClass "div" "my-2 w-full h-px bg-gray-200" blank

-- TODO(skylar): Use Requester from reflex
requester :: (Reflex t, EventWriter t [ClientMsg] m, MonadHold t m) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> Event t ClientMsg -> m (Event t (Tagged ServerMsg))
requester lastTagId serverMsg clientMsg = do
  waitingTag <- holdDyn Nothing $ current (Just <$> lastTagId) <@ clientMsg

  let
    isWhatWeAreWaitingFor (Just nid) msg@(Tagged nid' _) | nid == nid' = Just msg
    isWhatWeAreWaitingFor _ _ = Nothing

    properServerMsg = attachWithMaybe isWhatWeAreWaitingFor (current waitingTag) serverMsg

  tellEvent $ pure <$> clientMsg
  pure $ properServerMsg

headCreation ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
headCreation lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Head Creation"

    elClass "p" "" $ do
      text "To create a Head you will give it a friendly name and list the addresses that will become the participants."
      text " Creating the head starts the Hydra network."

  elClass "div" "p-4 bg-white rounded flex flex-col" $ do
    let
    (domEvent Click -> tryButtonClick, request) <- elClass "div" "flex flex-row justify-between items-end" $ do
      req <- elClass "div" "flex flex-row justify-between mb-4" $ do
        elClass "div" "flex flex-col" $ do
          elClass "div" "font-semibold flex flex-row" $ do
            elClass "div" "mr-2" $ text "Payload"
            elClass "div" "text-green-400" $ text "Object"

          name <- elClass "div" "ml-4 flex flex-row" $ do
            elClass "div" "mr-2" $ text "name"
            elClass "div" "font-semibold text-orange-400" $ text "String"
            elClass "div" "mx-4" $ text ":"
            elClass "div" "flex flex-col" $ do
              ie <- inputElement $ def
                & initialAttributes .~ ("class" =: "border px-2" <> "placeholder" =: "Enter a name for your head")
                & inputElementConfig_initialValue .~ "test"
              pure $ _inputElement_value ie

          participants <- elClass "div" "ml-4 flex flex-col" $ do
            elClass "div" "flex flex-row" $ do
              elClass "div" "mr-2" $ text "participants"
              elClass "div" "font-semibold text-orange-400" $ text "[String]"
              elClass "div" "mx-4" $ text ":"
            elClass "div" "flex flex-col" $ do
              ta <- textAreaElement $ def
                & initialAttributes .~ ("class" =: "border px-2" <> "type" =: "number" <> "placeholder" =: "Enter addresses separated by a newline" <> "cols" =: "65")
              pure $ fmap T.strip . T.lines <$> _textAreaElement_value ta
          pure $ fmap CreateHead $ HeadCreate <$> name <*> participants

      (tryEl, _) <- elClass' "div" "mb-4" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"
      pure (tryEl, req)

    elClass "div" "" $ do
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
        rec
          (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
            elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $ do
              elClass "span" "text-sm material-symbols-rounded" $ dynText $ ffor showTagged $ \case
                True -> "done"
                False -> ""
            elClass "div" "" $ text "Show Tagged"
          showTagged <- toggle False $ domEvent Click taggedToggle

        let
          reqJson = ffor3 showTagged lastTagId request $ \tagged nid req ->
            case tagged of
              True -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
              False -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

        dynText reqJson

    properServerMsg <- requester lastTagId serverMsg $ current request <@ tryButtonClick
    response <- holdDyn Nothing $ Just <$> properServerMsg

    dyn_ $ ffor response $ \case
      Just (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    expectedResponse $ OperationSuccess

theDevnet ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  )
  => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
theDevnet lastTagId serverMsg = do
  elClass "div" "p-4" $ do
    elClass "div" "text-xl mt-8 mb-2 font-semibold" $ text "The Devnet"
    elClass "div" "my-2 w-full h-px bg-gray-200" blank

    elClass "p" "" $ do
      text "When you run your Hydra Pay instance as a Devnet, you get access to this Live Documentation as well as special Devnet Requests that allow you to test features."
      text " After you have authenticated, refer back to these devnet requests to help you Create and Test Heads."
      el "br" blank
      el "br" blank
      text " For example, we are going to be creating and managing heads, it would be nice to have some addresses with funds to use, so we may request that from Hydra Pay."

  elClass "div" "p-4 bg-white rounded flex flex-col" $ do
    (domEvent Click -> tryButtonClick, request) <- elClass "div" "flex flex-row justify-between items-end" $ do
      req <- elClass "div" "flex flex-row justify-between mb-4" $ do
        elClass "div" "flex flex-col" $ do
          elClass "div" "font-semibold flex flex-row" $ do
            elClass "div" "mr-2" $ text "Payload"
            elClass "div" "text-green-400" $ text "Object"

          elClass "div" "ml-4 flex flex-row" $ do
            elClass "div" "mr-2" $ text "contents"
            elClass "div" "font-semibold text-orange-400" $ text "Number"
            elClass "div" "mx-4" $ text ":"
            elClass "div" "flex flex-col" $ do
              ie <- inputElement $ def
                & initialAttributes .~ ("class" =: "border px-2" <> "type" =: "number")
                & inputElementConfig_initialValue .~ "1"
              pure $ GetDevnetAddresses . maybe 1 id . readMaybe . T.unpack <$> _inputElement_value ie

      (tryEl, _) <- elClass' "div" "mb-4" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"
      pure (tryEl, req)

    elClass "div" "" $ do
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
        rec
          (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
            elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $ do
              elClass "span" "text-sm material-symbols-rounded" $ dynText $ ffor showTagged $ \case
                True -> "done"
                False -> ""
            elClass "div" "" $ text "Show Tagged"
          showTagged <- toggle False $ domEvent Click taggedToggle

        let
          reqJson = ffor3 showTagged lastTagId request $ \tagged nid req ->
            case tagged of
              True -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
              False -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

        dynText reqJson

    properServerMsg <- requester lastTagId serverMsg $ current request <@ tryButtonClick
    response <- holdDyn Nothing $ Just <$> properServerMsg

    dyn_ $ ffor response $ \case
      Just (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    responseVisualizer "Example Response" $ DevnetAddresses [ "addr_test1vpnpz04x65gmwcw25xr7p6spehmpmtakq885j92sprz7hggsnlm4a"
                                                            , "addr_test1vr6mp9zqpxk5nw7p3xm6kvnt2w6mgl9lhmzuvuez5v59hqgakn855"
                                                            ]


sayHello ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  )
  => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
sayHello lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Say Hello"

    elClass "p" "" $ do
      text "This Live Documentation is currently connected to your Hydra Pay instance, this allows you to easily verify if things are set up and working, and try requests and see responses."
      text " Lets say hello to the Hydra Pay instance, hit Try Request to send a ClientHello payload to Hydra Pay."

  let
    request = pure ClientHello

  elClass "div" "relative" $ do
    --elClass "div" "rounded-lg absolute z-10 w-full h-full backdrop-blur pointer-events-auto flex justify-center items-center" $ do
    --  text "Authenticate with Hydra Pay to use these"
    elClass "div" "p-4 bg-white rounded flex flex-col" $ do

      (domEvent Click -> tryButtonClick, _) <- elClass "div" "flex flex-row justify-between items-end" $ do
        elClass "div" "flex flex-row justify-between mb-4" $ do
          elClass "div" "font-semibold flex flex-row" $ do
            elClass "div" "mr-2" $ text "Payload"
            elClass "div" "text-green-400" $ text "Object"

        elClass' "div" "mb-4" $
          elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"

      elClass "div" "" $ do
        elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
          rec
            (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
              elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $ do
                elClass "span" "text-sm material-symbols-rounded" $ dynText $ ffor showTagged $ \case
                  True -> "done"
                  False -> ""
              elClass "div" "" $ text "Show Tagged"
            showTagged <- toggle False $ domEvent Click taggedToggle

          let
            reqJson = ffor3 showTagged lastTagId request $ \tagged nid req ->
              case tagged of
                True -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
                False -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

          dynText reqJson

      properServerMsg <- requester lastTagId serverMsg $ current request <@ tryButtonClick
      response <- holdDyn Nothing $ Just <$> properServerMsg

      dyn_ $ ffor response $ \case
        Just (Tagged _ msg@(AuthResult False)) -> do
          elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
          elClass "div" "" $ do
            elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
              text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

            elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
        Just (Tagged _ msg) -> do
          elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
          elClass "div" "" $ do
            elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
              text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

        Nothing -> blank

      expectedResponse $ ServerHello versionStr

responseVisualizer :: DomBuilder t m => T.Text -> ServerMsg -> m ()
responseVisualizer name msg = do
  -- Divider
  elClass "div" "mt-6 w-full h-px bg-gray-200" blank

  elClass "div" "font-semibold mt-4 mb-2" $ text name
  elClass "div" "" $ do
    elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
      text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

expectedResponse :: DomBuilder t m => ServerMsg -> m ()
expectedResponse = responseVisualizer "Expected Response"

authentication ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  )
  => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
authentication lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Authentication"

    elClass "p" "" $ do
      text "When you launch or deploy a Hydra Pay instance you will need to provide an API Key to authenticate against, this is a secret that should be only known to your DApp/LightWallet and your Hydra Pay instance."
      text "Upon opening a websocket connection to your HydraPay instance, you should immediately Authenticate by sending a Tagged `Authenticate` request (see below)."

  elClass "div" "p-4 bg-white rounded flex flex-col relative" $ do
    (tryButton, _) <- elClass "div" "flex flex-row justify-between" $ do
      elClass "div" "flex flex-row justify-between" $ do
        elClass "div" "font-semibold flex flex-row" $ do
          elClass "div" "mr-2" $ text "Payload"
          elClass "div" "text-green-400" $ text "Object"

      elClass' "div" "" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"

    let
      tryButtonClick = domEvent Click tryButton

    authRequest <- elClass "div" "ml-4 mb-4 flex flex-row" $ do
      elClass "div" "mr-2" $ text "contents"
      elClass "div" "font-semibold text-orange-400" $ text "String"
      elClass "div" "mx-4" $ text ":"
      elClass "div" "flex flex-col" $ do
        ie <- inputElement $ def
          & initialAttributes .~ ("class" =: "border px-2" <> "placeholder" =: "Enter your api key")
        pure $ Authenticate <$> _inputElement_value ie

    properServerMsg <- requester lastTagId serverMsg $ current authRequest <@ tryButtonClick
    response <- holdDyn Nothing $ Just <$> properServerMsg

    elClass "div" "" $ do
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
        rec
          (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
            elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $ do
              elClass "span" "text-sm material-symbols-rounded" $ dynText $ ffor showTagged $ \case
                True -> "done"
                False -> ""
            elClass "div" "" $ text "Show Tagged"
          showTagged <- toggle False $ domEvent Click taggedToggle

        let
          reqJson = ffor3 showTagged lastTagId authRequest $ \tagged nid req ->
            case tagged of
              True -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
              False -> decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

        dynText reqJson

    dyn_ $ ffor response $ \case
      Just (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    expectedResponse $ AuthResult True

  elClass "p" "p-4"$ do
    elClass "span" "text-gray-600 font-semibold" $ do
      text "Ensure you have authenticated before trying the requests below"


monitorView ::
  ( PostBuild t m
  , Prerender t m
  , SetRoute t (R FrontendRoute) (Client m)
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , DomBuilder t m
  , MonadFix m
  , MonadHold t m
  ) => m ()
monitorView = elClass "div" "w-full h-full text-gray-700 max-w-4xl mx-auto rounded flex flex-col" $ do
  prerender_ (text "Loading Live Documentation") $ do
    endpoint <- liftJSM $ do
      let
        webSocketProtocol :: T.Text -> T.Text
        webSocketProtocol "https:" = "wss:"
        webSocketProtocol _ = "ws:"

      wsProtocol <- fmap webSocketProtocol $ fromJSValUnchecked =<< jsg "location" ^. js "protocol"

      hostname <- fromJSValUnchecked =<< jsg "location" ^. js "hostname"
      port <- fromJSValUnchecked =<< jsg "location" ^. js "port"

      pure $ mconcat [ wsProtocol
                     , "//"
                     , hostname
                     , ":"
                     , port
                     , "/hydra/api"
                     ]
    pb <- getPostBuild

    rec
      lastTagId <- foldDyn (+) 0 $ fromIntegral . length <$> sendToHydraPay

      let
        sendToHydraPay =
          attachWith (\tid msgs -> fmap (uncurry Tagged) $ zip [tid..] msgs) (current lastTagId) sendMsg

        serverMsg = fmapMaybe id $ rws ^. webSocket_recv

      rws :: RawWebSocket t (Maybe (Tagged ServerMsg)) <- jsonWebSocket endpoint $ def
        & webSocketConfig_send .~ sendToHydraPay

      (_, sendMsg :: Event t [ClientMsg]) <- runEventWriterT $ do
        elClass "div" "p-4" $ do
          elClass "div" "text-3xl font-bold flex flex-row items-center justify-between" $ do
            el "div" $ text "🐲 Hydra Pay Live Documentation"
            elClass "div" "px-4 py-2 bg-gray-900 rounded text-white font-semibold text-sm" $ text $ "v" <> versionStr
          -- Divider
          elClass "div" "mt-4 w-full h-px bg-gray-200" blank

          elClass "div" "text-xl mt-8 mb-2 font-semibold" $ text "Websocket API"
          elClass "div" "my-2 w-full h-px bg-gray-200" blank
          elClass "p" "" $ do
            text "The Hydra Pay API is a websocket based API that gives you all that you need to manage and monitor heads, securely. The endpoint to connect to the Hydra Pay websocket is /hydra/api."
            text "Once you are connected you must authenticate via your Authentication Token you have set for your Hydra Pay Instance."
            text "In this section Client refers to the developer, and Server refers to the running Hydra Pay instance reachable at /hydra/api/."
            el "br" blank
            el "br" blank
            text " In this Live Documentation we Tag the requests automatically, and when viewing the JSON payload of a request you may always see the tagged equivalent, we even auto increment the request id,"
            text " this makes it easy to ensure you are sending the right information to Hydra Pay."

          -- Header
          elClass "div" "text-xl mt-8 mb-2 font-semibold" $ text "Tagging"
          elClass "div" "my-2 w-full h-px bg-gray-200" blank

          elClass "p" "" $ do
            text "All communication the Client does with the Server through the WebSocket must be Tagged."
            text " To Tag a message, we need the unique request-id, if we were sending this as the first message to our Server we may use 0 as the request-id."
            text " Remember that all communication the Client makes with the Server must be tagged. This forms the request response part of the API."

        authentication lastTagId serverMsg
        sayHello lastTagId serverMsg

        theDevnet lastTagId serverMsg

        headCreation lastTagId serverMsg


    elClass "div" "" blank

    rec
      mApiKey <- holdDyn Nothing $ fmap Just $ gotApiKey
      gotApiKey <- (switchHold never =<<) $ dyn $ ffor mApiKey $ \case
        Nothing -> do
          --text "Please enter your API Key"
          --           & initialAttributes .~ ("class" =: "pl-4 pr-2 py-1 bg-transparent text-right" <> "placeholder" =: "Hydra Pay API Key")
          pure never

        Just apiKey -> do
          pure never

          {-
          (authEl, _) <- elClass' "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Authenticate"
          (statsEl, _) <- elClass' "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Get Current Stats"
          (buttonEl, _) <- elClass' "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Restart Devnet"

          lastTagId <- foldDyn (+) 0 $ fromIntegral . length <$> sendToHydraPay

          let
            authenticate = Authenticate apiKey <$ domEvent Click authEl
            restartDevnet = RestartDevnet <$ domEvent Click buttonEl
            getStats = GetStats <$ domEvent Click statsEl

            sendMsg =
              mergeWith (<>) $ (fmap . fmap) pure $ [authenticate, restartDevnet, getStats]

            sendToHydraPay =
              attachWith (\tid msgs -> fmap (uncurry Tagged) $ zip [tid..] msgs) (current lastTagId) sendMsg

            serverMsg = fmapMaybe id $ rws ^. webSocket_recv

          endpoint <- liftJSM $ do
            let
              webSocketProtocol :: T.Text -> T.Text
              webSocketProtocol "https:" = "wss:"
              webSocketProtocol _ = "ws:"

            wsProtocol <- fmap webSocketProtocol $ fromJSValUnchecked =<< jsg "location" ^. js "protocol"

            hostname <- fromJSValUnchecked =<< jsg "location" ^. js "hostname"
            port <- fromJSValUnchecked =<< jsg "location" ^. js "port"

            pure $ mconcat [ wsProtocol
                           , "//"
                           , hostname
                           , ":"
                           , port
                           , "/hydra/api"
                           ]

           rws :: RawWebSocket t (Maybe (Tagged ServerMsg)) <- jsonWebSocket endpoint $ def
            & webSocketConfig_send .~ sendToHydraPay

          responses <- foldDyn (:) [] serverMsg
          display responses
          pure never -}
    pure ()

appView :: forall t m.
  ( PostBuild t m
  , Prerender t m
  , SetRoute t (R FrontendRoute) (Client m)
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , MonadIO (Performable m)
  , PerformEvent t m
  , TriggerEvent t m
  , DomBuilder t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t (Maybe T.Text) -> Dynamic t (Maybe T.Text) -> Dynamic t (Map Int DemoTx) -> RoutedT t (R FrontendRoute) m (Event t DemoTx)
appView bobAddress aliceAddress latestTxs = do
  fmap switchDyn $ subRoute $ \case
    FrontendRoute_Monitor -> monitorView >> pure never
    FrontendRoute_Setup -> do
      elClass "div" "text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"
        elClass "div" "text-lg" $ text "populating addresses..."
        pure never

    FrontendRoute_OpeningChannel -> do
      elClass "div" "text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"

        let
          messages = [ "Sending funds from Bob and Alice into Hydra Pay"
                     , "Creating payment channel"
                     , "Waiting for payment channel"
                     ]

        tick <- tickLossyFromPostBuildTime 2
        rec
          currentIndex <- holdDyn (0 :: Int) $ fmap (min (length messages - 1) . (+1)) $ current currentIndex <@ tick
        elClass "div" "text-lg" $ do
          dynText $ (messages !!) <$> currentIndex
          text "..."

        prerender_ blank $ mdo
            ws :: RawWebSocket t (Maybe DServerMsg) <- lift $ jsonWebSocket "ws://localhost:8000/demo-api" $ (WebSocketConfig @t @DClientMsg) wssend never True []
            let wssend = [DDemoInit] <$ _webSocket_open ws
            setRoute $ FrontendRoute_PaymentChannel :/ () <$ ffilter (== Just DInitDone) (_webSocket_recv ws)
      pure never

    FrontendRoute_ClosingChannel -> do
      elClass "div" "text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"

        let
          messages = [ "Closing payment channel"
                     , "Settling payment channel on L1"
                     , "Withdrawing funds from payment channel"
                     ]

        tick <- tickLossyFromPostBuildTime 5
        rec
          currentIndex <- holdDyn (0 :: Int) $ fmap (min (length messages - 1) . (+1)) $ current currentIndex <@ tick
        elClass "div" "text-lg" $ do
          dynText $ (messages !!) <$> currentIndex
          text "..."

        prerender_ blank $ mdo
            ws :: RawWebSocket t (Maybe DServerMsg) <- lift $ jsonWebSocket "ws://localhost:8000/demo-api" $ (WebSocketConfig @t @DClientMsg) wssend never True []
            let wssend = [DCloseFanout] <$ _webSocket_open ws
            setRoute $ FrontendRoute_OpenChannel :/ () <$ ffilter (== Just DCloseFanoutDone) (_webSocket_recv ws)
      pure never

    FrontendRoute_OpenChannel -> do
      -- Page Title
      elClass "div" "mb-2 font-semibold" $ text "Open a Payment Channel"
      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Balances
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "Current Balances"

      elClass "div" "flex flex-row mt-4" $ do
        balanceWidget "Bob" bobAddress never "l1-balance"
        balanceWidget "Alice" aliceAddress never "l1-balance"

      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Open Payment Channel UI
      elClass "div" "mt-8" $ do
        elClass "div" "text-sm mb-1" $ text "Amount to Spend"
        _ :: Dynamic t (Maybe Float) <- el "div" $ elClass "div" "w-fit border-2 border-gray-200 flex flex-row items-center" $ do
          amountInput <- inputElement $ def
            & initialAttributes .~ ("class" =: "pl-4 pr-2 py-1 bg-transparent text-right" <> "placeholder" =: "1 ADA" <> "type" =: "number")
            & inputElementConfig_initialValue .~ "1000"
          elClass "span" "mx-2 my-1" $ text "ADA"
          pure $ readMaybe . T.unpack <$> _inputElement_value amountInput
        elClass "div" "text-xs my-1" $ text "100 ADA in fast payment collateral will also be held in hydra pay"
        pure ()

        routeLink (FrontendRoute_OpeningChannel :/ ()) $ elClass "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Open Payment Channel"
      pure never

    FrontendRoute_SendFunds -> mdo
      -- Page Header
      elClass "div" "relative w-full flex flex-row items-baseline justify-between" $ do
        elClass "div" "mb-2 font-semibold" $ text "Bob & Alice's Payment Channel"
        (backButton, _) <- elClass' "button" "absolute -left-8 rounded w-6 h-6 bg-gray-300 text-center font-semibold" $ text "<"
        -- NOTE(skylar): Route link prevents the absolute-ness being applied properly, so we just use setRoute!
        setRoute $ FrontendRoute_PaymentChannel :/ () <$ domEvent Click backButton

      -- Divider
      elClass "div" "w-full h-px bg-gray-200" blank

      -- Balances
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ do
        text "Current Balances "
        elClass "span" "text-sm" $ text " In payment channel"

      elClass "div" "flex flex-row mt-4" $ do
        balanceWidget "Bob" bobAddress never "head-balance"
        balanceWidget "Alice" aliceAddress never "head-balance"

      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Send
      elClass "div" "text-2xl mb-4 font-semibold mt-8" $ text "Send ADA"

      -- To/From Selector
      rec
        (selectorButton, _) <- elClass' "button" "w-fit bg-gray-300 px-4 py-2 rounded flex gap-6 flex-row" $ do
          elClass "div" "" $ do
            elClass "div" "text-left text-xs" $ text "From"
            elClass "div" "font-semibold" $ dynText $ ffor bobIsTo $ \case
              True -> "Alice"
              False -> "Bob"

          elClass "div" "" $ do
            elClass "div" "text-left text-xs" $ text "To"
            elClass "div" "font-semibold" $ dynText $ toName

        let
          toName = ffor bobIsTo $ \case
            True -> "Bob"
            False -> "Alice"

          bobIsTo = ((==) <$> toAddr <*> bobAddress)
          changeSelection = current nextSelection <@ domEvent Click selectorButton
          fromAddr = ffor3 bobIsTo bobAddress aliceAddress $ \isBob bob alice ->
            if isBob then alice else bob
          nextSelection = fromAddr

        sendBuild <- getPostBuild
        toAddr <- holdDyn Nothing $ leftmost [current bobAddress <@ sendBuild, changeSelection]
        pure ()

      lovelaceSendAmount :: Dynamic t (Maybe Integer) <- el "div" $ elClass "div" "mt-4 w-full border-2 border-gray-200 flex flex-row items-center" $ do
          amountInput <- inputElement $ def
            & initialAttributes .~ ("class" =: "text-gray-500 w-full px-8 py-6 bg-transparent text-center text-xl" <> "placeholder" =: "1 ADA" <> "type" =: "number")
            & inputElementConfig_initialValue .~ "10"
          elClass "span" "mx-2 my-1" $ text "ADA"
          pure $ fmap ((round :: Float -> Integer) . ada) . readMaybe . T.unpack <$> _inputElement_value amountInput

      (sendButton, _) <- elClass' "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Send ADA"

      let
        txSendPayload = liftA2 (HeadSubmitTx "demo") <$> toAddr <*> lovelaceSendAmount
        sendAda = fmapMaybe (preview _Just) $ current txSendPayload <@ domEvent Click sendButton
        sendUrl = (fmap ("/submit-tx/"<>) <$> fromAddr)
        sendReq = liftA2 postJson <$> sendUrl <*> txSendPayload

        sendSuccess = fmapMaybe (preview _Just) $ (decodeText <=< _xhrResponse_responseText) <$> sendAdaResponse

        latestTxFunc = liftA2 DemoTx <$> (Just <$> toName) <*> lovelaceSendAmount

      -- Dynamic function to construct the last transaction
      picoToLatestTx <- holdDyn Nothing $ attachPromptlyDynWith const latestTxFunc sendAda

      sendAdaResponse <- fmap switchDyn $ prerender (pure never) $ do
        performRequestAsync $ fmapMaybe (preview _Just) $ current sendReq <@ sendAda

      pure $ attachWithMaybe (\mf a -> mf <*> Just a) (current picoToLatestTx) sendSuccess

    FrontendRoute_PaymentChannel -> do
      -- Page Title
      elClass "div" "w-full flex flex-row items-baseline justify-between" $ do
        elClass "div" "mb-2 font-semibold" $ text "Bob & Alice's Payment Channel"
        routeLink (FrontendRoute_ClosingChannel :/ ()) $ elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold" $ text "Close Payment Channel"

      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "L1 Balance"

      elClass "div" "flex flex-row mt-4" $ do
        balanceWidget "Bob" bobAddress never "l1-balance"
        balanceWidget "Alice" aliceAddress never "l1-balance"

      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Balances
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ do
        text "Payment Channel Balance"

      rec
        (bobBalance, aliceBalance) <- elClass "div" "flex flex-row mt-4" $ do
          bb <- balanceWidget "Bob" bobAddress autoTx "head-balance"
          ab <- balanceWidget "Alice" aliceAddress autoTx "head-balance"
          pure (bb, ab)

        -- Divider
        elClass "div" "mt-2 w-full h-px bg-gray-200" blank

        -- Statistics
        elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "Statistics"

        elClass "div" "flex flex-row mt-4" $ do
          elClass "div" "mr-16" $ do
            elClass "div" "text-lg font-semibold" $ text "Total Transactions"
            elClass "div" "text-3xl text-gray-600" $ dynText $ T.pack . show . Map.size <$> latestTxs

          elClass "div" "mr-16" $ do
            elClass "div" "text-lg font-semibold" $ text "Total Time"
            elClass "div" "text-3xl text-gray-600" $ do
              dynText $ showAsMs . Map.foldr ((+) . demoTx_time) 0 <$> latestTxs
              text "ms"

        -- Divider
        elClass "div" "mt-2 w-full h-px bg-gray-200" blank

        -- Action buttons: Send & Automate
        (automateButton, _) <- elClass "div" "" $ do
          routeLink (FrontendRoute_SendFunds :/ ()) $ elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold mr-4" $ text "Send ADA"
          elClass' "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold" $ dynText $ ffor automating $ \case
            True -> "Automating"
            False -> "Automate"

        let
          toggleAutomate = domEvent Click automateButton

        automating <- foldDyn ($) False $ not <$ toggleAutomate

        autoTxEv <- dyn $ ffor automating $ \case
          False -> pure never
          True -> fmap switchDyn $ prerender (pure never) $ do
            tick <- tickLossyFromPostBuildTime 1
            randomAmount :: Event t Lovelace <- performEvent $ (liftIO $ ada <$> randomRIO (1,10)) <$ tick
            let
              nextToAddr = join $ ffor2 bobBalance aliceBalance $ \bb ab ->
                if bb > ab then aliceAddress else bobAddress

              nextFromAddr = join $ ffor2 bobBalance aliceBalance $ \bb ab ->
                if bb > ab then bobAddress else aliceAddress

              sendUrl = (fmap ("/submit-tx/"<>) <$> nextFromAddr)

              bobIsTo = ((==) <$> nextToAddr <*> bobAddress)
              toName = ffor bobIsTo $ \case
                True -> "Bob"
                False -> "Alice"

              changeDemoBuildTxFunc = attachWith DemoTx (current toName) randomAmount

              submitPayload = attachWith (<*>) (current $ fmap (HeadSubmitTx "demo") <$> nextToAddr) $ Just <$> randomAmount
              submitReq = attachWithMaybe (<*>) (current $ fmap postJson <$> sendUrl) submitPayload

            picoToLatestTx <- holdDyn Nothing $ Just <$> changeDemoBuildTxFunc
            sendAdaResponse <- performRequestAsync submitReq

            let
              sendSuccess = fmapMaybe (preview _Just) $ (decodeText <=< _xhrResponse_responseText) <$> sendAdaResponse

            pure $ attachWithMaybe (<*>) (current picoToLatestTx) $ Just <$> sendSuccess

        autoTx <- switchHold never autoTxEv

      -- Transaction History
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "History"

      let
        historyItem tx = do
          elClass "div" "transition pointer-events-none duration-1000 px-4 py-2 rounded bg-white mb-4 drop-shadow " $ do
            elClass "div" "flex flex-row justify-between items-center" $ do
              elClass "div" "flex flex-col mr-16" $ do
                elClass "div" "text-gray-600" $ do
                  text "To "
                  dynText $ demoTx_to <$> tx
                elClass "div" "text-xl" $ do
                  dynText $ T.pack . printf "%.2f" . lovelaceToAda . demoTx_amount <$> tx
                  text "ADA"

              elClass "div" "flex flex-row text-gray-400" $ do
                elClass "div" "flex flex-col" $ do
                  elClass "div" "text-xs" $ text "Time"
                  elClass "div" "" $ do
                    dynText $ showAsMs . demoTx_time <$> tx
                    text "ms"

      let
        noHistory = Map.null <$> latestTxs

      elDynClass "div" "overflow-y-scroll flex-grow" $ dyn_ $ ffor noHistory $ \case
        True -> elClass "div" "text-gray-500" $ text "There is no history yet for this payment channel"
        False -> do
          _ <- elClass "div" "flex flex-col-reverse w-full" $ listWithKey latestTxs $ \_ tx -> historyItem tx
          pure ()

      pure autoTx
