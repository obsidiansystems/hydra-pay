{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend
  ( frontend
  )
where

import Prelude hiding (filter)

import Data.Int
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Encode.Pretty as Aeson

import Hydra.Types
import HydraPay.Api

import Text.Read (readMaybe)

import Data.Fixed
import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route

import Reflex.Dom.Core
import Common.Route

import Control.Lens
import Control.Monad.Fix

import Language.Javascript.JSaddle (jsg, js, liftJSM, fromJSValUnchecked)
import Data.Bool (bool)

default (T.Text)

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

  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen overflow-y-scroll overflow-x-hidden flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $
      elClass "div" "w-full h-full text-gray-700 max-w-4xl mx-auto p-4 rounded flex flex-col" monitorView
  }

data DemoTx = DemoTx
  { demoTx_to :: T.Text
  , demoTx_amount :: Lovelace
  , demoTx_time :: Pico
  }
  deriving (Eq, Show)

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
  pure properServerMsg


data ResponseStatus
  = Standby
  | WaitingOnResponse
  | GotResponse (Tagged ServerMsg)

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
      el "br" blank
      el "br" blank
      elClass "span" "italic" $ text " This request may take some time as Hydra Pay waits for all Hydra Nodes to respond"

  elClass "div" "p-4 bg-white rounded flex flex-col" $ do
    let
    (tryButtonClick, request) <- elClass "div" "flex flex-row justify-between items-start" $ do
      req <- elClass "div" "flex flex-row justify-between mb-4" $
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
      pure (domEvent Click tryEl, req)

    elClass "div" "" $
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
      rec
        (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
          elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $ do
            elClass "span" "text-sm material-symbols-rounded" $ dynText $ ffor showTagged $ bool "" "done"
          elClass "div" "" $ text "Show Tagged"
        showTagged <- toggle False $ domEvent Click taggedToggle

      let
        reqJson = ffor3 showTagged lastTagId request $ \tagged nid req ->
          if tagged
            then decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
            else decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

      dynText reqJson

    let
      fireRequest = current request <@ tryButtonClick

    properServerMsg <- requester lastTagId serverMsg fireRequest
    response <- holdDyn Standby $ mergeWith const $ [WaitingOnResponse <$ fireRequest, GotResponse <$> properServerMsg]

    dyn_ $ ffor response $ \case
      GotResponse (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in config/backend/api-key"
      GotResponse (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
          text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      WaitingOnResponse -> do
        elClass "div" "font-semibold mt-4" $ text "Waiting for Response..."
        elClass "div" "" $
          elClass "pre" "animate-pulse relative rounded-lg px-4 py-8 border bg-gray-900 text-green-500" blank

      Standby -> blank

    expectedResponse OperationSuccess

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
    (domEvent Click -> tryButtonClick, request) <- elClass "div" "flex flex-row justify-between items-start" $ do
      req <- elClass "div" "flex flex-row justify-between mb-4" $
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

    elClass "div" "" $
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
          if tagged
            then decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
            else decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

      dynText reqJson

    properServerMsg <- requester lastTagId serverMsg $ current request <@ tryButtonClick
    response <- holdDyn Nothing $ Just <$> properServerMsg

    dyn_ $ ffor response $ \case
      Just (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
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

  elClass "div" "relative" $
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
            if tagged
            then decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
            else decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

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
  elClass "div" "" $
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
      elClass "div" "flex flex-row justify-between" $
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
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Enter your api key"
        pure $ Authenticate <$> _inputElement_value ie

    properServerMsg <- requester lastTagId serverMsg $ current authRequest <@ tryButtonClick
    response <- holdDyn Nothing $ Just <$> properServerMsg

    elClass "div" "" $
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
          if tagged
            then decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ Tagged nid req
            else decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ req

      dynText reqJson

    dyn_ $ ffor response $ \case
      Just (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in configs/backend/api-key"
      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
          text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    expectedResponse $ AuthResult True

  elClass "p" "p-4"$
    elClass "span" "text-gray-600 font-semibold" $ do
    text "Ensure you have authenticated before trying the requests below"


monitorView ::
  ( PostBuild t m
  , Prerender t m
  , DomBuilder t m
  ) => m ()
monitorView = elClass "div" "w-full h-full text-gray-700 max-w-4xl mx-auto rounded flex flex-col" $
  prerender_ (text "Loading Live Documentation") $ do
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

    let
      sendToHydraPay =
        attachWith (\tid msgs -> uncurry Tagged <$> zip [tid..] msgs) (current lastTagId) sendMsg

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
          text " Once you are connected you must authenticate via your Authentication Token you have set for your Hydra Pay Instance."
          text " In this section Client refers to the developer, and Server refers to the running Hydra Pay instance reachable at /hydra/api/."
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

  pure ()
