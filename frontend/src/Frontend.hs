{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Data.Bool
import Data.Traversable
import Data.Foldable

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

import Hydra.Types as HT

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Hydra Head Demo"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank
  , _frontend_body = do
      rec
        (_, requests) <- runRequesterT app responses
        responses <- performWebSocketRequests "ws://localhost:8000/api" requests
      pure ()
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
                    , Response (Client m) ~ Either T.Text) => m (Dynamic t WholeUTXO)
watchDevnetUTXOs = do
  tick <- fmap switchDyn $ prerender (pure never) $ tickLossyFromPostBuildTime 2
  result <- requestingJs $ DemoApi_GetWholeUTXO <$ tick
  holdDyn mempty $ fmapMaybe eitherToMaybe result

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

-- data HeadState
--   = Idle
--   | Initializing {parties :: [Party], remainingParties :: [Party], utxo :: UTxO}
--   | Open {parties :: [Party], utxo :: UTxO}
--   | Closed {contestationDeadline :: UTCTime}
--   | FanoutPossible
--   | Final {utxo :: UTxO}
--   deriving (Eq, Show, Generic)

-- | Tracks the state of the head based on Hydra Node responses
data HeadState
  = Idle
  | Initializing
  | Open
  deriving (Eq, Show)

wholeUtxoDisplay :: (DomBuilder t m, PostBuild t m) => Dynamic t WholeUTXO -> m ()
wholeUtxoDisplay wholeUtxo = do
  dyn_ $ ffor wholeUtxo $ Map.traverseWithKey $ \k v -> do
    elClass "div" "text-gray-200" $ do
      elClass "div" "p-2 flex flex-row justify-between" $ do
        elClass "div" "text-lg font-semibold" $ text $ T.pack $ address v
        when (isFuel v) $ elClass "div" "px-2 py-1 bg-green-500 text-white font-semibold text-sm rounded-full" $ text "FUEL"
      elClass "div" "p-2 text-sm text-gray-300 font-semibold border-b border-gray-700 flex flex-row justify-between" $ do
        elClass "div" "" $ text k
        elClass "div" "" $ text $ (maybe "" (T.pack . show) $  Map.lookup "lovelace" $ HT.value v) <> " lovelace"
    pure ()
    where
      isFuel v = maybe False (== fuelMarkerDatumHash) $ datumhash v

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

  elClass "div" "cursor-pointer hover:drop-shadow-xl transition-all drop-shadow m-4 rounded-lg bg-gray-800 text-white h-64 overflow-x-hidden overflow-y-scroll" $ do
    elClass "div" "backdrop-blur-md text-xl p-6 font-bold sticky top-0 border-b border-white" $ text "Current Devnet UTXOs"
    elClass "div" "p-6" $ watchDevnetUTXOs >>= wholeUtxoDisplay

  let
    participants =
      [ ("alice", "ws://localhost:9001")
      , ("bob", "ws://localhost:9002")
      , ("carol", "ws://localhost:9003")
      ]

  prerender_ blank $ mdo
    headState <- holdDyn Idle newState

    elClass "div" "ml-4 mt-8 mr-4 mb-2 w-full font-black text-green-500" $ dyn_ $ ffor headState $ \case
      Idle -> do
        elClass "div" "text-lg" $ text "Head State: IDLE"
        elClass "div" "text-green-700 text-sm" $ text "Waiting for participant to init..."

      Initializing -> do
        elClass "div" "text-lg" $ text "Head State: Initializing"
        elClass "div" "text-green-700 text-sm" $ text $ "Waiting for commits from: " <> (T.intercalate ", " $ fmap fst participants)

      _ -> blank

    newState <- elClass "div" "ml-4 mr-4 overflow-hidden rounded-lg hover:drop-shadow-xl transition-all drop-shadow bg-gray-800" $ mdo
      rec
        currentTab <- holdDyn (fst . head $ participants) changeTab

        changeTab <- fmap leftmost $ elClass "div" "w-full flex flex-row justify-start" $ for (fmap fst participants) $ \name -> do
          let
            isSelected = (== name) <$> currentTab
            mkClasses selected =
              T.intercalate " " [ "leading-none p-4 font-bold text-2xl text-gray-100 flex items-center justify-center"
                                , bool "bg-gray-800 text-gray-300 pointer-cursor" "bg-gray-700 text-gray-100" selected
                                ]
          (buttonEl, _) <- elDynClass' "button" (mkClasses <$> isSelected) $ text name
          pure $ name <$ domEvent Click buttonEl

      -- participants
      fmap leftmost $ forM participants
        $ \(name, wsUrl) -> do
            let
               isSelected = (== name) <$> currentTab
               mkClasses selected =
                 T.intercalate " " [ "p-2 bg-gray-700 text-white"
                                   , bool "hidden" "" selected
                                   ]
            elDynClass "div" (mkClasses <$> isSelected) $ mdo
              let wsCfg :: (WebSocketConfig t T.Text) = WebSocketConfig (fmap pure action) never False []
              ws <- textWebSocket wsUrl wsCfg
              let
                webSocketMessage = _webSocket_recv ws

                processLog msg
                  | T.isInfixOf "ReadyToCommit" msg  = Just Initializing
                  | T.isInfixOf "HeadIsOpen" msg = Just Open
                  | otherwise = Nothing

                stateChange = fmapMaybe processLog webSocketMessage

              actionEv :: _ <- dyn $ ffor headState $ \case
                Idle -> do
                  elClass "div" "p-8 flex flex-row items-center justify-center" $ do
                    (buttonEl, _) <- elClass' "button" "bg-blue-500 hover:bg-blue-400 active:bg-blue-300 text-white font-bold text-xl px-4 py-2 rounded-md" $ text $ "Initialize head as " <> name
                    pure $ "{ \"tag\": \"Init\", \"contestationPeriod\": 60 }" <$ domEvent Click buttonEl
                Initializing -> do
                  elClass "div" "p-8 flex flex-row items-center justify-center" $ do
                    text $ "Getting " <> name <> "'s UTXOs"
                  pure never
                _ ->
                  text "Not handled" >> pure never

              action <- switchHold never actionEv
              elClass "div" "mb-1 font-semibold text-sm" $ text "Hydra Node Log"
              elClass "div" "p-2 bg-gray-800 rounded-md drop-shadow" $ do
                el "ul" $ do
                  comms <- foldDyn (++) [] $
                    (fmap ((:[]) . ("Rcv: " <>)) $ webSocketMessage)
                    <>
                    fmap (fmap ("Snd: " <>)) never
                  dyn_ $ fmap (mapM (el "li" . text) . reverse) comms
              -- toSendDyn <- fmap (:[]) . _inputElement_value <$> inputElement (
              --  def & inputElementConfig_setValue .~ ("" <$ toSend))
              {-(buttonEl, _) <- elClass' "button" "" $ text "send"
              let toSend =
                    leftmost [--  ["{ \"tag\": \"Init\", \"contestationPeriod\": 60 }"] <$ _webSocket_open ws
                             -- ,
                               current toSendDyn <@ domEvent Click buttonEl
                             ]-}
              pure stateChange
    pure ()
