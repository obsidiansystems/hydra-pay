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

import Language.Javascript.JSaddle (MonadJSM, jsg, js, js1, liftJSM, fromJSValUnchecked)
import Data.Bool (bool)
import HydraPay.Config (HydraPayMode (ConfiguredMode), CardanoNodeParams (..))
import Data.String.Interpolate ( iii, __i )
import Control.Monad (when)
import Data.Maybe (fromMaybe)

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
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank

      -- Highlight JS for syntax highlighting
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css") blank
      elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js") blank

  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen overflow-hidden flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $
      prerender_ (text "Loading Live Documentation") $ do
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

    rws :: RawWebSocket t (Maybe ApiMsg) <- jsonWebSocket endpoint $ def
      & webSocketConfig_send .~ sendToHydraPay

    (_, sendMsg :: Event t [ClientMsg]) <- runEventWriterT $ elClass "div" "w-full h-full flex flex-row text-gray-700" $ do
      activeHeads authenticated lastTagId subscriptions responses
      managedDevnetReply <- requester lastTagId responses (GetIsManagedDevnet  <$ pb)
      _ <- runWithReplace blank $
        mapMaybe ((\case
                      IsManagedDevnet x -> Just (monitorView lastTagId responses x)
                      _ -> Nothing)
                   . tagged_payload)
        managedDevnetReply
      elClass "div" "flex-grow" $ elClass "div" "max-w-lg" blank
  pure ()
  }

data ResponseStatus
  = Standby
  | WaitingOnResponse
  | GotResponse (Tagged ServerMsg)

data TrySectionConfig t m = TrySectionConfig
  { trySection_payloadInput :: m (Dynamic t ClientMsg)
  , trySection_example :: m ()
  , trySection_genHelpfulErrorText :: ServerMsg -> Maybe T.Text
  , trySection_extraStuff :: Dynamic t ClientMsg -> ServerMsg -> m ()
  }

copyToClipboard :: MonadJSM m => T.Text -> m ()
copyToClipboard payload = do
  _ <- liftJSM $ jsg "navigator" ^. js "clipboard" . js1 "writeText" payload
  pure ()

trySection ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> TrySectionConfig t m ->  m ()
trySection lastTagId serverMsg config = do
  -- (TrySectionConfig pinput example genErrorText extraStuff)
  elClass "div" "p-4 bg-white rounded flex flex-col" $ do
    (tryButtonClick, request) <- elClass "div" "flex flex-row justify-between mb-4" $ do
      req <- trySection_payloadInput config
      (tryEl, _) <- elClass' "div" "mb-4" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"
      pure (domEvent Click tryEl, req)

    elClass "div" "" $
      elClass "pre" "overflow-x-scroll relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
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

    let
      fireRequest = current request <@ tryButtonClick

    properServerMsg <- requester lastTagId serverMsg fireRequest

    response <- holdDyn Standby $ mergeWith const [WaitingOnResponse <$ fireRequest, GotResponse <$> properServerMsg]

    dyn_ $ ffor response $ \case
      GotResponse (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "overflow-y-scroll relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          case trySection_genHelpfulErrorText config msg of
            Just errMsg -> elClass "div" "text-sm font-semibold text-red-400" $ text errMsg
            Nothing -> blank

        trySection_extraStuff config request msg

      WaitingOnResponse -> do
        elClass "div" "font-semibold mt-4" $ text "Waiting for Response..."
        elClass "div" "" $
          elClass "pre" "animate-pulse relative rounded-lg px-4 py-8 border bg-gray-900 text-green-500" blank

      Standby -> blank

    trySection_example config
  pure ()

removeHead ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
removeHead lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Removing a Head"

    elClass "p" "" $
      text "Once your Head is finalized, and your DApp no longer needs access to the Head information, you can remove the Head from Hydra Pay."

  let
    input = payloadInput $ do
      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "head name"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which Head would you like close?"
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie
      pure $ TearDownHead <$> name

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (expectedResponse $ HeadRemoved "test")
    (const Nothing)
    noExtra

closeHead ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
closeHead lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Closing a Head"

    elClass "p" "" $
      text "After you have done all the transactions you needed to in the Hydra Head, you can close it and when Hydra Pay detects the Fanout period has occurred, it will Fanout your Head for you Automatically."

  let
    input = payloadInput $ do
      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "head name"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which Head would you like close?"
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie
      pure $ CloseHead <$> name

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (expectedResponse OperationSuccess)
    (const Nothing)
    noExtra

activeHeads ::
  ( PostBuild t m
  , DomBuilder t m
  , PerformEvent t m
  , MonadJSM (Performable m)
  , MonadHold t m
  , MonadFix m
  ) => Dynamic t Bool -> Dynamic t Int64 -> Event t ServerMsg -> Event t (Tagged ServerMsg) -> m ()
activeHeads authenticated _ subscriptions responses =
  elClass "div" "flex-grow p-4" $ elClass "div" "max-w-lg" $ do
  header "Your Heads"

  dyn_ $ ffor authenticated $ \case
    False -> elClass "span" "text-gray-400" $ text "Authenticate to monitor heads"
    True -> do
      subscribed <- foldDyn ($)  (mempty :: Set T.Text) $ Set.insert . headStatus_name <$> startedSubscription
      heads <- foldDyn ($) (mempty :: Map T.Text HeadStatus) $
        mergeWith (.) [ uncurry Map.insert <$> gotHeadInfo
                      , uncurry Map.insert . (\x -> (headStatus_name x, x)) <$> startedSubscription
                      , (\(hname, status, balances) -> Map.adjust (\hstatus -> hstatus { headStatus_status = status
                                                                                       , headStatus_balances =
                                                                                          Map.union balances (headStatus_balances hstatus)
                                                                                       }
                                                                  ) hname) <$> statusChange
                      , (\(hname, balances) -> Map.adjust (\hstatus -> hstatus { headStatus_balances =
                                                                                   Map.union balances (headStatus_balances hstatus)
                                                                               }) hname) <$> balanceChange
                      , Map.delete <$> headGone
                      ]
      _ <- listWithKey heads (headStatusWidget subscribed)
      pure ()
  where
    headStatusWidget subs name hstatus = do
      let
        isOpen = (== Status_Open) . headStatus_status <$> hstatus

      elClass "div" "w-full rounded-lg mb-4 shadow-md bg-white flex flex-col" $ do
        elClass "div" "flex p-4 flex-row" $ do
          elClass "div" "leading-none pr-4 border-r-2 overflow-hidden" $ do
            elClass "span" "text-xs text-gray-400" $ text "HEAD"
            elClass "div" "text-2xl w-full font-semibold overflow-hidden truncate" $ text name
          elClass "div" "pl-4 flex flex-col flex-grow flex-shrink-0" $ do
            elClass "div" "flex flex-row justify-between items-center" $ do
              elClass "span" "text-gray-400 mr-16" $ text "Status"
              elClass "span" "text-green-500" $ dynText $ prettyStatus . headStatus_status <$> hstatus
            elClass "div" "flex flex-row justify-between items-center" $ do
              elClass "span" "text-gray-400 mr-16" $ text "Network Running"
              elClass "span" "text-green-500" $ dynText $ tShow . headStatus_running <$> hstatus
            elClass "div" "flex flex-row justify-between items-center" $ do
              elClass "span" "text-gray-400 mr-16" $ text "Subscribed"

              let
                mkClasses = bool "text-red-500" "text-green-500"
                isSubbedToHead = Set.member name <$> subs
              elDynClass "span" (mkClasses <$> isSubbedToHead) $ dynText $ tShow <$> isSubbedToHead

        dyn_ $ ffor (Map.null . headStatus_balances <$> hstatus) $ \case
          True -> blank
          False -> do
            elClass "div" "flex flex-col mt-2" $ do
              elClass "div" "px-4 pt-4" $ do
                elClass "div" "w-full h-px bg-gray-200" blank
                elClass "span" "mt-2 text-sm text-gray-400 font-semibold" $ dynText $ ffor isOpen $ \case
                  True -> "LIVE BALANCES"
                  False -> "PARTICIPANTS"
                elClass "div" "mt-1 w-full h-px bg-gray-200" blank
              _ <- listWithKey (headStatus_balances <$> hstatus) $ \addr lovelace -> do
                (addrButton, _) <- elClass' "button" "rounded-lg mx-2 px-2 py-1 mb-2 hover:bg-gray-200 active:bg-gray-400 flex flex-row justify-between items-baseline" $ do
                  elClass "div" "font-semibold text-gray-600" $ do
                    text $ T.take 12 $ unAddress addr
                    text "..."
                    text $ T.takeEnd 8 $ unAddress addr

                  elClass "div" "flex flex-row items-baseline" $ do
                    let
                      mkAdaClasses b =
                        mconcat
                          [ "font-bold text-lg text-green-700 "
                          , bool "hidden" "" b
                          ]
                    elDynClass "div" (mkAdaClasses <$> isOpen) $ dynText $ T.pack . printf "%.2f" . lovelaceToAda <$> lovelace
                    elClass "span" "ml-2 text-lg material-symbols-rounded" $ text "content_copy"
                performEvent_ $ copyToClipboard (unAddress addr) <$ domEvent Click addrButton
              pure ()


    untagged = tagged_payload <$> responses

    gotHeadInfo = (\hs -> (headStatus_name hs, hs)) <$> fmapMaybe (preview _HeadInfo) untagged
    startedSubscription = fmapMaybe (preview _SubscriptionStarted) untagged
    statusChange = fmapMaybe (preview _HeadStatusChanged) subscriptions
    balanceChange = fmapMaybe (preview _BalanceChange) subscriptions
    headGone = fmapMaybe (preview _HeadRemoved) subscriptions

    prettyStatus = \case
      Status_Pending -> "Pending"
      Status_Init -> "Initialized"
      Status_Committing -> "Waiting for Commits"
      Status_Open -> "Open"
      Status_Closed -> "Closed"
      Status_Fanout -> "Fanning Out"
      Status_Finalized -> "Finalized"
      Status_Aborted -> "Aborted"

header :: DomBuilder t m => T.Text -> m ()
header name = do
  elClass "div" "text-xl mt-8 mb-2 font-semibold" $ text name
  elClass "div" "my-2 w-full h-px bg-gray-200" blank

requester :: (Reflex t, EventWriter t [ClientMsg] m, MonadHold t m) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> Event t ClientMsg -> m (Event t (Tagged ServerMsg))
requester lastTagId serverMsg clientMsg = do
  waitingTag <- holdDyn Nothing $ current (Just <$> lastTagId) <@ clientMsg

  let
    isWhatWeAreWaitingFor (Just nid) msg@(Tagged nid' _) | nid == nid' = Just msg
    isWhatWeAreWaitingFor _ _ = Nothing

    properServerMsg = attachWithMaybe isWhatWeAreWaitingFor (current waitingTag) serverMsg

  tellEvent $ pure <$> clientMsg
  pure properServerMsg

sendFundsInHead ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
sendFundsInHead lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Transactions in a Head"

    elClass "p" "" $ do
      text "Once a Head is Open (all participants have commited UTxOs), you are free to move funds around within the head by creating transactions."
      text " You will get BalanceChanged payloads if you are subscribed to the Head. We are using this payload in the Live Documentation to live update the balance of all participants in the Head in the \"Your Heads\" section."

  let
    input = payloadInput $ do
      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "head name"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which Head are sending funds in?"
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie
      toAddr <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "to"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Who is the sender?"
          pure $ UnsafeToAddress <$> _inputElement_value ie
      fromAddr <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "from"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Who is the recipient?"
          pure $ UnsafeToAddress <$> _inputElement_value ie

      amount <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "lovelace"
        elClass "div" "font-semibold text-orange-400" $ text "Number"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Amount in lovelace"
          & inputElementConfig_initialValue .~ "3000000"
        pure $ fromMaybe (ada 3) . readMaybe . T.unpack <$> _inputElement_value ie

      let
        mkSubmitHeadTx hname toaddr fromaddr lovelace =
          SubmitHeadTx fromaddr (HeadSubmitTx hname toaddr lovelace)

      pure $ mkSubmitHeadTx <$> name <*> toAddr <*> fromAddr <*> amount

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (exampleResponse $ TxConfirmed 0.001)
    (const Nothing)
    noExtra

  pure ()

commitToHead ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
commitToHead lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Committing to a Head"

    elClass "p" "" $ do
      text "Once a Head is initialized each Participant must commit funds to be used within it."
      text " The participant committing to the Head must have an ouptut that matches the amount you put"
      elClass "span" "italic" $ text " exactly."

  let
    input = payloadInput $ do
      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "head name"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which Head are you Initializing?"
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie
      addr <- elClass "div" "ml-4 flex flex-row mb-4" $ do
        elClass "div" "mr-2" $ text "address"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Committer address"
        pure $ UnsafeToAddress <$> _inputElement_value ie
      amount <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "lovelace"
        elClass "div" "font-semibold text-orange-400" $ text "Number"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Amount in lovelace"
          & inputElementConfig_initialValue .~ "100000000"
        pure $ fromMaybe (ada 3) . readMaybe . T.unpack <$> _inputElement_value ie
      pure $ fmap CommitHead $ HeadCommit <$> name <*> addr <*> amount

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (expectedResponse OperationSuccess)
    (const Nothing)
    noExtra

  pure ()

initHead ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
initHead lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Initializing The Head"

    elClass "p" "" $ do
      text "The next step, once we have fuel and funds for all our participants, is to initialize a Head."
      text " This creates an L1 transaction (using fuel given to a Proxy Address) that places the initial head state on chain."

  let
    input = payloadInput $ do
      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "head name"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which Head are you Initializing?"
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie
      contestationPeriod <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "contestation period (seconds)"
        elClass "div" "font-semibold text-orange-400" $ text "Number"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Time in seconds"
          & inputElementConfig_initialValue .~ "3"
        pure $ fromMaybe 3 . readMaybe . T.unpack <$> _inputElement_value ie
      pure $ fmap InitHead $ HeadInit <$> name <*> contestationPeriod

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (expectedResponse OperationSuccess)
    (const Nothing)
    noExtra

  pure ()

proxyAddressInfo ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) ->  m ()
proxyAddressInfo lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Proxy Address Information"

    elClass "p" "" $ do
      text "Sometimes you want to inspect or be aware of the Proxy Address' funds and fuel, maybe to automate filling those up in your business logic."
      text " The Proxy Info includes the address of the proxy address, and the balance and fuel of that proxy address."

  let
    input = payloadInput $ do
      addr <- elClass "div" "ml-4 flex flex-row mb-4" $ do
        elClass "div" "mr-2" $ text "address"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which address do you want proxy info for?"
        pure $ _inputElement_value ie
      pure $ GetProxyInfo . UnsafeToAddress <$> addr

    example =
      ProxyAddressInfo $ ProxyInfo
      obviouslyInvalidAddress
      obviouslyInvalidAddress
      50000000
      50000000

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (exampleResponse example)
    (const Nothing)
    noExtra
  pure ()

fundingProxyAddresses ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> Bool -> m ()
fundingProxyAddresses lastTagId serverMsg isManagedDevnet = do
  elClass "div" "px-4 pb-4" $ do
    header "Funding Proxy Addresses | Funds & Fuel"

    elClass "p" "" $
      text "To participate in a Hydra Pay managed Head you need to transfer funds from your address to your Proxy Address. You must transfer both regular Ada and a specially tagged Fuel transaction yourself."
    elClass "p" "" $
      text "For your convenience Hydra Pay provides endpoints which return draft transactions for both types of funds. However, you yourself must sign and submit these transactions with the signing key for your address."

    hintText $ text $
      bool
      "You're running Hydra Pay with an externally managed Cardano network. After you request the funding transactions, we'll provide shell commands for you to submit them."
      "Since you're running Hydra Pay in managed-devnet mode, we can conveniently submit transactions for you. Give it a try!"
      isManagedDevnet

  let
    input = payloadInput $ do
      rec
        txType <- holdDyn Funds changeType
        changeType <- elClass "div" "ml-4 flex flex-row items-center mb-2" $ do
          elClass "div" "mr-2" $ text "tx type"
          elClass "div" "font-semibold text-orange-400" $ text "String"
          elClass "div" "mx-4" $ text ":"
          elClass "div" "flex flex-row items-stretch rounded-lg border-green-600 border-2" $ do
            let
              mkClasses b =
                mconcat [ "text-white flex-grow-0 px-4 py-1 text-center font-bold "
                        , bool "text-green-600" "text-white bg-green-400" b
                        ]

              currentlyFunds = (Funds ==) <$> txType
              currentlyFuel = (Fuel ==) <$> txType

            (fundsButton, _) <- elDynClass' "button" (mkClasses <$> currentlyFunds) $ text "Funds"
            elClass "div" "w-px bg-green-600" blank
            (fuelButton, _) <- elDynClass' "button" (mkClasses <$> currentlyFuel) $ text "Fuel"
            pure $ leftmost [Funds <$ domEvent Click fundsButton, Fuel <$ domEvent Click fuelButton]
      addr <- elClass "div" "ml-4 flex flex-row mb-4" $ do
        elClass "div" "mr-2" $ text "address"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Which address will you add funds from"
        pure $ UnsafeToAddress <$> _inputElement_value ie
      amount <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "lovelace"
        elClass "div" "font-semibold text-orange-400" $ text "Number"
        elClass "div" "mx-4" $ text ":"
        ie <- inputElement $ def
          & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Amount in lovelace"
          & inputElementConfig_initialValue .~ "100000000"
        pure $ fromMaybe (ada 3) . readMaybe . T.unpack <$> _inputElement_value ie

      pure $ GetAddTx <$> txType <*> addr <*> amount

  let
    shellCommands cardanoParams = \case
      FundsTx tx -> elClass "div" "my-4" $ do
        elClass "div" "font-semibold my-4" $ text "Submitting the transaction"
        elClass "p" "my-4" $ text "To submit the transaction on your Cardano net you can execute the shell commands below. Remember to fill in the path to your signing key file."
        elClass "pre" "overflow-y-scroll" $ text
          [__i|
              cat > unsigned-tx.json<< EOF
                #{Aeson.encodePretty tx}
              EOF

              cardano-cli transaction sign \\
                --tx-body-file unsigned-tx.json \\
                --out-file signed.json \\
                --signing-key-file [PATH-TO-SIGNING-KEY]

              CARDANO_NODE_SOCKET_PATH=#{_nodeSocket cardanoParams} \\
                cardano-cli transaction submit \\
                --tx-file signed.json \\
                --testnet-magic #{_testnetMagic cardanoParams}
              |]
      _ -> blank
    submitTxButton req = \case
      FundsTx tx -> elClass "div" "mt-4" $ do
        rec
          let
            submit = current (LiveDocEzSubmitTx tx <$> properReq) <@ clickedSubmit
            submitFailureMessage = ffor (tagged_payload <$> properServerMsg) $ \case
              OperationSuccess -> Nothing
              ApiError err -> Just err
              ServerError err -> Just $ tShow err
              _ -> Just "An Unknown Error Occured"

          submitStatus <- holdDyn TxNotSubmitted $ leftmost [TxDone <$ properServerMsg, TxSubmitting <$ submit]
          errorMsg <- holdDyn Nothing submitFailureMessage
          clickedSubmit <- elClass "div" "" $ do
            evEv <- dyn $ ffor submitStatus $ \case
              TxSubmitting -> do
                elClass "div" "text-green-600 font-semibold" $ text "Submitting Transaction on Devnet..."
                pure never
              TxNotSubmitted -> do
                (submitButton, _) <- elClass' "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Submit Tx"
                pure $ domEvent Click submitButton
              TxDone -> do
                dyn_ $ ffor errorMsg $ \case
                  Nothing -> elClass "div" "text-green-600 font-semibold" $ text "Tx Submitted"
                  Just msg -> elClass "div" "text-sm font-semibold text-red-400" $ do
                    text "Tx Failed: "
                    text msg

                    text " generate a new tx if you have submitted one recently, or restart hydra-pay"
                pure never

            switchHold never evEv
          properServerMsg <- requester lastTagId serverMsg submit
        pure ()

      _ -> blank
      where
        properReq = ffor req $ \case
          GetAddTx _ addr _ -> addr
          _ -> UnsafeToAddress ""

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (exampleResponse $ FundsTx $ Tx
    "Unwitnessed Tx BabbageEra"
    "84a3008182582005bbe2c33e4bd787a8778b63bfbf007fae7b47b8153e75586df0ab59936d6c3c000182a300581d60e04a63ce5112f1b75c66a13375daf937e5ed9177caa8e9536392119f011a002dc6c00282005820a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3a200581d60d31a9209c0da931b7e72f45bc612dc85fae49249619f5f80639d2f50011b0000000253db8f8b021a00028db5a0f5f6"
    "Ledger Cddl Format"
    )
    (const Nothing)
    (if isManagedDevnet
      then submitTxButton
      else \_req thisServerMsg -> do
        pb <- getPostBuild
        hydraPayModeReply <- requester lastTagId serverMsg (GetHydraPayMode <$ pb)
        _ <- runWithReplace blank $
          mapMaybe ((\case
                            HydraPayMode (ConfiguredMode cardanoParams _) -> Just (shellCommands cardanoParams thisServerMsg)
                            _ -> Nothing)
                         . tagged_payload)
              hydraPayModeReply
        pure ())

  pure ()

proxyAddresses ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> Bool -> m ()
proxyAddresses lastTagId serverMsg isManagedDevnet = do
  elClass "div" "px-4 pb-4" $ do
    header "Proxy Addresses"

    elClass "p" "" $ do
      text "Hydra Pay simplifies the creation and managment of Heads to facilitate easy creation of Hydra Head based features for Light Wallet and DApp developers."
      text " One way we aid feature creation is through our Proxy Address structure."
      el "br" blank
      el "br" blank
      text "Instead of participating directly in a Head, any participant will actually be mapped to a \"Proxy Address\"."
      text " This is a regular cardano address that is created to hold funds and fuel for said participant in a Hydra Pay Head."

  fundingProxyAddresses lastTagId serverMsg isManagedDevnet
  proxyAddressInfo lastTagId serverMsg

data SubmitStatus
  = TxNotSubmitted
  | TxSubmitting
  | TxDone

payloadInput :: DomBuilder t m => m a -> m a
payloadInput input =
  elClass "div" "flex flex-row justify-between mb-4" $
  elClass "div" "flex flex-col" $ do
  elClass "div" "font-semibold flex flex-row" $ do
    elClass "div" "mr-2" $ text "Payload"
    elClass "div" "text-green-400" $ text "Object"
  input

hintText :: DomBuilder t m => m a -> m a
hintText = elClass "div" "text-gray-400 font-semibold italic my-4"

withdrawFromHydraPay ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
withdrawFromHydraPay lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Withdrawing from Hydra Pay (Proxy Address)"

    elClass "p" "" $ do
      text "After you are done using your Funds within Hydra Pay, you are free to withdraw them from a Proxy address."
      text " You can take back everything, or just funds or just fuel."
      text " This allows flexibility in how DApp developers and potentially individual users want to structure withdrawals"

    hintText $ text "Remember withdrawal is a L1 transaction where the Proxy address sends funds back to the Non Proxy. This is subject to fees on networks that aren't the Devnet."

  let
    input = do
      elClass "div" "flex flex-row justify-between mb-4" $ elClass "div" "flex flex-col" $ do
        elClass "div" "font-semibold flex flex-row" $ do
          elClass "div" "mr-2" $ text "Payload"
          elClass "div" "text-green-400" $ text "Object"

        addr <- elClass "div" "ml-4 flex flex-row" $ do
          elClass "div" "mr-2" $ text "address"
          elClass "div" "font-semibold text-orange-400" $ text "String"
          elClass "div" "mx-4" $ text ":"
          elClass "div" "flex flex-col" $ do
            ie <- inputElement $ def
              & initialAttributes .~ ("class" =: "border px-2" <> "placeholder" =: "Which address would you like to withdraw to?")
            pure $ UnsafeToAddress <$> _inputElement_value ie

        takeFuel <- elClass "div" "ml-4 flex flex-row items-center" $ do
          elClass "div" "mr-2" $ text "take fuel"
          elClass "div" "font-semibold text-orange-400" $ text "Bool"
          elClass "div" "mx-4" $ text ":"
          elClass "div" "flex flex-col" $ do
            ie <- inputElement $ def
              & initialAttributes .~ ("class" =: "border px-2" <> "type" =: "checkbox")
            pure $ _inputElement_checked ie

        pure $ Withdraw <$> addr <*> takeFuel

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (exampleResponse $ WithdrawSubmitted "e174c1033009de66ccc577743ae4542c9d5e6c8220acfcd55c1c4cf330b7ca04")
    (const Nothing)
    noExtra


subscribeTo ::
  ( EventWriter t [ClientMsg] m
  , DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> m ()
subscribeTo lastTagId serverMsg = do
  elClass "div" "px-4 pb-4" $ do
    header "Subscribing to State Changes"

    elClass "p" "" $ do
      text "A lot of the time the logic for your DApp or Light Wallet will include waiting for certain state changes to take place."
      text " To be convenient for developers and to avoid unecessary complications and resource usage of polling, you can subscribe to state changes of certain heads."
      text " If you create a head you should subscribe to it as well."

    hintText $ text "When you subscribe to updates for a head in the live documentation, we will automatically update the view in \"Your Heads\"."

  let
    input =
      elClass "div" "flex flex-row justify-between mb-4" $
      elClass "div" "flex flex-col" $ do
      elClass "div" "font-semibold flex flex-row" $ do
        elClass "div" "mr-2" $ text "Payload"
        elClass "div" "text-green-400" $ text "Object"

      name <- elClass "div" "ml-4 flex flex-row" $ do
        elClass "div" "mr-2" $ text "contents"
        elClass "div" "font-semibold text-orange-400" $ text "String"
        elClass "div" "mx-4" $ text ":"
        elClass "div" "flex flex-col" $ do
          ie <- inputElement $ def
            & initialAttributes .~ ("class" =: "border px-2" <> "placeholder" =: "Which head would you like to monitor?")
            & inputElementConfig_initialValue .~ "test"
          pure $ _inputElement_value ie

      pure $ SubscribeTo <$> name

  trySection lastTagId serverMsg $
    TrySectionConfig
    input
    (exampleResponse
     $ SubscriptionStarted
     $ HeadStatus "test" True Status_Pending mempty)
    (const Nothing)
    noExtra

noExtra :: Monad m => a -> b -> m ()
noExtra _ _ = blank

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

    hintText $ text " This request may take some time as Hydra Pay waits for all Hydra Nodes to respond."

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
              & initialAttributes .~ "class" =: "border px-2" <> "placeholder" =: "Enter a name for your head"
              & inputElementConfig_initialValue .~ "test"
            pure $ _inputElement_value ie

        participants <- elClass "div" "ml-4 flex flex-col" $ do
          elClass "div" "flex flex-row" $ do
            elClass "div" "mr-2" $ text "participants"
            elClass "div" "font-semibold text-orange-400" $ text "[String]"
            elClass "div" "mx-4" $ text ":"
          elClass "div" "flex flex-col" $ do
            ta <- textAreaElement $ def
              & initialAttributes .~ "class" =: "border px-2" <> "type" =: "number" <> "placeholder" =: "Enter addresses separated by a newline" <> "cols" =: "65"
            pure $ fmap (UnsafeToAddress . T.strip) . T.lines <$> _textAreaElement_value ta
        pure $ fmap CreateHead $ HeadCreate <$> name <*> participants

      (tryEl, _) <- elClass' "div" "mb-4" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"
      pure (domEvent Click tryEl, req)

    elClass "div" "" $
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
      rec
        (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
          elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $
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
    response <- holdDyn Standby $ mergeWith const [WaitingOnResponse <$ fireRequest, GotResponse <$> properServerMsg]

    dyn_ $ ffor response $ \case
      GotResponse (Tagged _ msg@(AuthResult False)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $ do
          elClass "pre" "overflow-x-scroll relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

          elClass "div" "text-sm font-semibold text-red-400" $ text "It looks like your API Key is invalid, ensure your key matches the one hydra pay was given in config/backend/api-key"
      GotResponse (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
          elClass "pre" "overflow-x-scroll relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
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
  , MonadJSM (Performable m)
  , PerformEvent t m
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
              & initialAttributes .~ "class" =: "border px-2" <> "type" =: "number"
              & inputElementConfig_initialValue .~ "2"
            pure $ GetDevnetAddresses . fromMaybe 1 . readMaybe . T.unpack <$> _inputElement_value ie

      (tryEl, _) <- elClass' "div" "mb-4" $ elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"
      pure (tryEl, req)

    elClass "div" "" $
      elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
      rec
        (taggedToggle, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $ do
          elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $
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

      Just (Tagged _ msg@(DevnetAddresses addrs)) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $ do
            (copyButton, _) <- elClass' "button" "absolute text-white top-0 right-0 p-2 flex flex-row items-center" $
              elClass "div" "p-2 rounded active:bg-white/30 hover:bg-gray-400/30 bg-black/30 flex justify-center items-center text-center" $ do
              elClass "span" "text-3xl material-symbols-rounded" $ text "content_copy"

            performEvent_ $ copyToClipboard (T.intercalate "\n" . fmap unAddress $ addrs) <$ domEvent Click copyButton

            text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg
        elClass "div" "mt-4" $
          for_ (zip [1..] addrs) $ \(n :: Integer, addr) -> do
          (addrButton, _) <- elClass' "button" "w-full hover:bg-gray-200 active:bg-gray-700 active:text-white mb-2 px-2 py-1 border rounded-md font-semibold text-md flex flex-row items-center justify-between" $ do
            elClass "div" "" $ do
              elClass "span" "font-bold mr-2" $ text $ tShow n
              text $ unAddress addr
            elClass "span" "text-2xl material-symbols-rounded" $ text "content_copy"
          performEvent_ $ copyToClipboard (unAddress addr) <$ domEvent Click addrButton

      Just (Tagged _ msg) -> do
        elClass "div" "font-semibold mt-4" $ text "Hydra Pay Responded"
        elClass "div" "" $
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
          text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    responseVisualizer "Example Response" $ DevnetAddresses [ obviouslyInvalidAddress
                                                            , obviouslyInvalidAddress
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
      text " Let's say hello to the Hydra Pay instance. Hit Try Request to send a ClientHello payload to Hydra Pay."

  let
    request = pure ClientHello

  elClass "div" "relative" $
    elClass "div" "p-4 bg-white rounded flex flex-col" $ do

    (domEvent Click -> tryButtonClick, _) <- elClass "div" "flex flex-row justify-between items-end" $ do
      elClass "div" "flex flex-row justify-between mb-4" $
        elClass "div" "font-semibold flex flex-row" $ do
        elClass "div" "mr-2" $ text "Payload"
        elClass "div" "text-green-400" $ text "Object"

      elClass' "div" "mb-4" $
        elClass "button" "flex-grow-0 rounded-md px-4 py-1 text-center bg-green-500 text-white font-bold border-2 border-green-600" $ text "Try Request"

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

    expectedResponse $ ServerHello versionStr

responseVisualizer :: DomBuilder t m => T.Text -> ServerMsg -> m ()
responseVisualizer name msg = do
  -- Divider
  elClass "div" "mt-6 w-full h-px bg-gray-200" blank

  elClass "div" "font-semibold text-sm text-gray-400 mt-4 mb-2" $ text name
  elClass "div" "" $
    elClass "pre" "overflow-x-scroll relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
    text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

expectedResponse :: DomBuilder t m => ServerMsg -> m ()
expectedResponse = responseVisualizer "Expected Response"

exampleResponse :: DomBuilder t m => ServerMsg -> m ()
exampleResponse = responseVisualizer "Example Response"

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

    elClass "p" "" $
      text [iii|
             When you launch or deploy a Hydra Pay instance you will
             need to provide an API Key to authenticate against.
             This is a secret that should be only known to your
             DApp/LightWallet and your Hydra Pay instance.
             Upon opening a websocket connection to your HydraPay instance,
             you should immediately Authenticate by sending a Tagged `Authenticate` request (see below).
           |]

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
          elClass "div" "rounded w-4 h-4 bg-orange-500 mr-2 flex justify-center items-center text-center" $
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
          elClass "pre" "relative rounded-lg p-4 border bg-gray-900 text-green-500" $ elClass "code" "language-json" $
          text $ decodeUtf8 . LBS.toStrict . Aeson.encodePretty $ msg

      Nothing -> blank

    expectedResponse $ AuthResult True

  elClass "p" "p-4 my-8"$
    elClass "span" "text-gray-600 font-semibold" $
    text "Ensure you have authenticated before trying the requests below."

monitorView ::
  ( PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadJSM (Performable m)
  , PerformEvent t m
  , EventWriter t [ClientMsg] m
  ) => Dynamic t Int64 -> Event t (Tagged ServerMsg) -> Bool -> m ()
monitorView lastTagId serverMsg isManagedDevnet = do
  elClass "div" "h-full text-gray-700 max-w-4xl overflow-y-scroll rounded flex flex-col flex-shrink-0" $ do
    elClass "div" "p-4" $ do
      elClass "div" "text-3xl font-bold flex flex-row items-center justify-between" $ do
        el "div" $ text "🐲 Hydra Pay Live Documentation"
        elClass "div" "px-4 py-2 bg-gray-900 rounded text-white font-semibold text-sm" $ text $ "v" <> versionStr
      -- Divider
      elClass "div" "mt-4 w-full h-px bg-gray-200" blank

      elClass "p" "my-4" $
        text [iii|
               This page provides hands-on documentation of the Hydra
               Pay API. You can try out every request against your
               configured Cardano network. If you didn't specify any,
               Hydra Pay will have started a devnet for you with
               pre-funded addresses. This page is fully implemented
               using the Hydra Pay API and thus also serves as an
               example of its use.
               |]

      elClass "div" "text-xl mt-12 mb-2 font-semibold" $ text "Websocket API"
      elClass "div" "my-2 w-full h-px bg-gray-200" blank
      elClass "p" "" $ do
        text "The Hydra Pay API is a websocket based API that gives you all that you need to manage and monitor heads, securely. The endpoint to connect to the Hydra Pay websocket is /hydra/api."
        text " Once you are connected you must authenticate via the Authentication Token you have set for your Hydra Pay Instance."
        text " In this section Client refers to the developer, and Server refers to the running Hydra Pay instance reachable at /hydra/api/."

      -- Header
      elClass "div" "text-lg mt-8 mb-2 font-semibold" $ text "Tagging"

      elClass "p" "my-4" $
        text [iii|
          The Hydra Pay WebSocket API comes with a convenient scheme to keep track of responses to specific requests.
          Each request must be tagged with an identifying value as follows:|]
      elClass "pre" "my-4" . text $ [__i|{ "tagged_payload": REQUEST, "tagged_id": ID }|]
      elClass "p" "my-4" $ text [iii|When the server replies this identifier will be included:|]
      elClass "pre" "my-4" . text $ [__i|{ "tagged_payload": REPLY, "tagged_id": ID }|]

      elClass "p" "my-4" $
        text [iii|
          In this Live Documentation we tag the requests
          automatically and hide the tagging by default. When viewing the JSON payload of a
          request you can always look at the tagged equivalent by checking "Show Tagged".
          |]

    authentication lastTagId serverMsg

    sayHello lastTagId serverMsg

    when isManagedDevnet $ theDevnet lastTagId serverMsg

    headCreation lastTagId serverMsg

    subscribeTo lastTagId serverMsg

    proxyAddresses lastTagId serverMsg isManagedDevnet

    initHead lastTagId serverMsg

    commitToHead lastTagId serverMsg

    sendFundsInHead lastTagId serverMsg

    closeHead lastTagId serverMsg

    removeHead lastTagId serverMsg

    withdrawFromHydraPay lastTagId serverMsg
  pure ()
