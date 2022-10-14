{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}

module Frontend
  ( frontend
  )
where

import Prelude hiding (filter)
import Control.Monad
import Control.Monad.Fix
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Read (readMaybe)

import Data.Bool
import Data.Traversable

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Common.Api
import Common.Route

import Reflex.Dom.GadtApi

import Hydra.Types as HT
import Data.Aeson as Aeson
import Data.Witherable
import Hydra.ServerOutput
import Hydra.ClientInput
import Data.Time (UTCTime, diffUTCTime)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.String.Interpolate (i,__i)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import Data.Semigroup (First(..))
import Control.Monad.IO.Class (MonadIO)


alicebobcarolDemo :: HydraDemo
alicebobcarolDemo = Map.fromList [("Alice", (1000000000, 100000000)), ("Bob", (500000000, 100000000)), ("Carol", (250000000, 100000000))]


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
  :: ( MonadFix m
     , Prerender t m
     , Requester t (Client m)
     , Request (Client m) ~ DemoApi
     , Response (Client m) ~ Either T.Text
     )
  => Event t (Request (Client m) a) -- ^ 
  -> m (Event t (Response (Client m) a))
requestingJs r = fmap (switch . current) $ prerender (pure never) $ requesting r

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing

filterOutFuel :: WholeUTXO -> WholeUTXO
filterOutFuel = Map.filter (not . isFuel)

isFuel :: TxInInfo -> Bool
isFuel txinfo = datumhash txinfo == Just fuelMarkerDatumHash

-- | Tracks the state of the head based on Hydra Node responses
data HeadState
  = Idle
  | Initializing
  | Open
  | Closed UTCTime
  | StateReadyToFanout
  deriving (Eq, Show)


buttonClass :: (PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m b -> m (Event t ())
buttonClass cls content = do
  (buttonEl, _) <- elDynClass' "button" cls content
  pure $ domEvent Click buttonEl


utxoPicker :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Bool -> WholeUTXO -> m (Dynamic t (Maybe WholeUTXO))
utxoPicker pickable wholeUtxo = mdo
  elClass "div" "font-semibold text-lg mb-2" $ text "UTxOs"

  currentUtxo <- holdDyn Nothing selectedUtxo
  selectedUtxo <- fmap (leftmost . Map.elems) $ elClass "div" "flex flex-row flex-wrap gap-2" $ flip Map.traverseWithKey wholeUtxo $ \k v -> mdo
      let amiSelected = maybe False ((k ==) . fst) <$> currentUtxo
      let cls = ("text-white font-bold text-xl px-4 py-2 rounded-md flex flex-row cursor-pointer mr-2 " <>)
            . bool
              "bg-gray-500 hover:bg-gray-400 active:bg-gray-300"
              "bg-blue-500 hover:bg-blue-400 active:bg-blue-300"
            <$> amiSelected
      (buttonEl, _) <- elDynClass' "button" cls $ do
        elClass "div" "text-sm text-gray-300 font-semibold flex justify-between" $ do
          elClass "div" "flex flex-col" $ do
            elClass "div" "w-full flex flex-row justify-between" $ do
              elClass "div" "text-gray-400 mr-4" $ text "lovelace"
              when (isFuel v) $ elClass "div" "px-2 py-0 flex items-center justify-center leading-node bg-green-500 text-xs text-white font-semibold text-sm rounded-full flex" $
                el "div" $ text "FUEL"
            elClass "div" "text-lg text-left font-semibold" $ text $ maybe "" (T.pack . show) (Map.lookup "lovelace" $ HT.value v)

      pure $ bool Nothing (Just (k, v)) . (pickable &&) . not <$> current amiSelected <@ domEvent Click buttonEl
  pure $ fmap (uncurry Map.singleton) <$> currentUtxo

demoSettings :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => HydraDemo -> m (Dynamic t HydraDemo)
demoSettings setngs = elClass "div" "flex flex-col pl-4 pr-4" $ do
  let initSize = Map.size setngs
  let initialList = Map.fromList (zip [1 .. ] (Map.toList setngs))
  rec
    nextIdentityNumber <- fmap (1 + initSize +) <$> count newNode
    let updates =
          ((\n -> Map.singleton n (Just ([i|Node #{n}|], (100000000, 100000000))))  <$> current nextIdentityNumber <@ newNode)
          <> deleteEs
    (((), deleteEs), demoDyn) <- runDynamicWriterT $ runEventWriterT $ void $ elClass "div" "flex-col space-y-2" $ do
      elClass "p" "text-white text-2xl my-4" $ text "Configure a Hydra Head by specifying the node names and their initial funds in Lovelace."
      listHoldWithKey initialList updates $ \k (name, (actorSeed, _hydraSeed)) -> elClass "div" "flex flex-col" $ do
        name' <- elClass "div" "flex flex-row space-x-2" $ do
          name' <- fmap _inputElement_value . inputElement $
            def & inputElementConfig_initialValue .~ name
            & initialAttributes .~ ("class" =: "text-white bg-gray-800 text-2xl font-bold focus:outline-none p-2" <> "type" =: "text")
          amount' <- fmap _inputElement_value . inputElement $
            def & inputElementConfig_initialValue .~ (T.pack . show $ actorSeed)
            & initialAttributes .~ ("class" =: "text-white bg-gray-800 text-2xl font-bold focus:outline-none p-2" <> "type" =: "number")
          deleteE <- buttonClass "bg-gray-400 hover:bg-gray-300 active:bg-gray-200 text-white font-bold text-xl px-4 py-2 rounded-md" $
                     text "×"
          let actorSeed' = (\n a -> (n,) <$> readMaybe (T.unpack a)) <$> name' <*> amount'
          tellDyn (maybe mempty (Map.singleton k. (\(actor,sd) -> (actor, (sd, 100000000)))) <$> actorSeed')
          tellEvent (Map.singleton k Nothing <$ deleteE)
          pure name'
        let hasDuplicateName = (\n ns -> (> 1) . Map.size . Map.filter (\(n',_) -> n == n') $ ns) <$> name' <*> demoDyn
        let duplicateNameMsg = elClass "div" "text-red-400 m-2" $ text "Duplicate name"
        dyn_ (bool blank duplicateNameMsg <$> fromUniqDynamic (uniqDynamic hasDuplicateName))
        pure ()
    newNode <- buttonClass "bg-gray-400 hover:bg-gray-300 active:bg-gray-200 text-white font-bold text-xl my-4 px-4 py-2 rounded-md w-32" $
             text "Add node"
    let demoDyn' = Map.fromList . Map.elems <$> demoDyn
  pure demoDyn'

startStopDemoControls ::
  ( Requester t (Client m),
    DomBuilder t m,
    MonadFix m,
    PostBuild t m,
    Prerender t m,
    Response (Client m) ~ Either T.Text,
    Request (Client m) ~ DemoApi,
    MonadHold t m
  ) =>
  m (Event t RunningNodes)
startStopDemoControls = mdo
  headRunning <- toggle False headStartedOrStoppedE
  ((), demoConfig) <- runDynamicWriterT $ dyn_ (bool (tellDyn =<< demoSettings alicebobcarolDemo) blank <$> headRunning)
  startStopHeadE <- buttonClass ((\running ->
                                    let color :: Text = bool "green" "red" running
                                    in [__i|bg-#{color}-500 hover:bg-#{color}-400 active:bg-#{color}-300
                                           text-white font-bold text-xl m-4 px-4 py-2 rounded-md|]
                                      :: Text)
                                  <$> headRunning)
                    $ dynText (bool "Start head" "Stop head" <$> headRunning)
  let startStopWithConfE = current demoConfig <@ startStopHeadE
  headStartedOrStoppedE <- fmap (mapMaybe eitherToMaybe)
    . requestingJs $
    -- Start with mempty to stop the demo:
    (\running conf -> DemoApi_Start $ bool conf mempty running)
    <$> current headRunning
    <@> startStopWithConfE
  let headStartingDom conf =
        if Map.null conf
        then blank
        else elClass "div" "text-white text-2xl m-4" $ text "Head starting..."
  void $ runWithReplace blank $ leftmost [ headStartingDom <$> startStopWithConfE
                                         , blank <$ headStartedOrStoppedE
                                         ]
  pure headStartedOrStoppedE


app :: forall t m.
       ( Prerender t m
       , PostBuild t m
       , DomBuilder t m
       , Requester t (Client m)
       , Request (Client m) ~ DemoApi
       , Response (Client m) ~ Either T.Text
       ) => m ()
app = elClass "div" "w-screen h-screen bg-gray-900 overflow-y-scroll overflow-x-hidden" $ do
  elClass "div" "p-4 m-4 text-white text-5xl font-bold" $ text "Hydra Proof Of Concept Demo"
  prerender_ blank $ mdo
    headStartedE <- startStopDemoControls
    void $ runWithReplace blank $ ffor headStartedE $ \actors -> mdo
      let actorNames = ffor (Map.toList actors) $ \(name, (_,_)) -> name
      headState <- holdDyn Idle newState
      let headStateDom = elClass "div" "text-lg" . text . ("Head State: " <>)
      unless (null actors) $ elClass "div" "ml-4 mt-8 mr-4 mb-2 w-full font-black text-green-500" $ dyn_ $ ffor headState $ \case
        Idle -> do
          headStateDom "Idle"
          elClass "div" "text-green-700 text-sm" $ text "Waiting for participant to init..."
        Initializing -> do
          headStateDom "Initializing"
          elClass "div" "text-green-700 text-sm" $ text $ "Waiting for commits from: " <> T.intercalate ", " actorNames
        Open -> headStateDom "Open"
        Closed _ -> headStateDom "Closed/Contestation period"
        StateReadyToFanout -> headStateDom "Ready to fanout"

      newState <- elClass "div" "ml-4 mr-4 overflow-hidden rounded-lg hover:drop-shadow-xl transition-all drop-shadow bg-gray-800" $ mdo
        rec
          currentTab <- holdDyn (head actorNames) changeTab

          changeTab <- fmap leftmost $ elClass "div" "w-full flex flex-row justify-start" $ for actorNames $ \name -> do
            let
              isSelected = (== name) <$> currentTab
              mkClasses selected =
                T.intercalate " " [ "leading-none p-4 font-bold text-2xl text-gray-100 flex items-center justify-center"
                                  , bool "bg-gray-800 text-gray-300 pointer-cursor" "bg-gray-700 text-gray-100" selected
                                  ]
            (buttonEl, _) <- elDynClass' "button" (mkClasses <$> isSelected) $ text name
            pure $ name <$ domEvent Click buttonEl
        fmap (fmap getFirst . snd) . runEventWriterT $ forM (Map.toList actors) $ \(name, (actorAddress, wsUrl)) -> mdo
              let wsCfg = (WebSocketConfig @t @ClientInput) action never True []
              ws <- jsonWebSocket wsUrl wsCfg
              let isSelected = (== name) <$> currentTab
              let mkClasses selected =
                     T.intercalate " " [ "p-2 bg-gray-700 text-white flex flex-col items-left"
                                       , bool "hidden" "" selected
                                       ]
              (_, action) <- elDynClass "div" (mkClasses <$> isSelected) $ runEventWriterT $ runWithReplace (elClass "div" "text-white" $ text "Connecting to node...") . ffor (_webSocket_open ws) $ \() -> do
                let
                  webSocketMessage :: Event t (ServerOutput Aeson.Value) =
                    fromMaybe (error "Parsing message from Hydra node failed") <$> _webSocket_recv ws
                  processLog = \case
                    ReadyToCommit {} -> Just Initializing
                    HeadIsOpen {} -> Just Open
                    HeadIsClosed _ fanoutTime -> Just (Closed fanoutTime)
                    ReadyToFanout {} -> Just StateReadyToFanout
                    HeadIsAborted {} -> Just Idle
                    HeadIsFinalized {} -> Just Idle
                    _ -> Nothing
                let stateChange = fmapMaybe processLog webSocketMessage
                let

                myVKeyB :: Behavior t (Maybe T.Text) <-
                   hold Nothing
                     . fmap Just
                     . mapMaybe (\case
                                    Greetings (Party vkey') -> Just vkey'
                                    _ -> Nothing)
                     $ webSocketMessage
                headStateE <- mdo
                  void $ dyn $ ffor headState $ \case
                    Idle -> idleScreen name
                    Initializing -> initializingScreen name actorAddress myVKeyB webSocketMessage
                    Open -> openScreen name actorNames actorAddress webSocketMessage
                    Closed fanoutTime -> closedScreen fanoutTime
                    StateReadyToFanout ->
                      tellAction
                        . (Fanout <$)
                        <=< buttonClass "bg-green-400 hover:bg-green-400 active:bg-green-200 text-white font-bold text-xl my-2 px-4 py-2 rounded-md w-32" $ text "Do fanout"
                  elClass "div" "mt-4" $ do
                    elClass "div" "mb-1 font-semibold text-sm" $ text "Hydra Node Log"
                    elClass "div" "p-2 bg-gray-800 rounded-md drop-shadow" $
                      el "ul" $ do
                      comms <- foldDyn (++) [] $
                        ((:[]) . ("Rcv: " <>) . toStrict . toLazyText . encodeToTextBuilder . toJSON <$> webSocketMessage)
                        <>
                        fmap (fmap (("Snd: " <>) . toStrict . toLazyText . encodeToTextBuilder . toJSON)) action
                      dyn_ $ mapM (el "li" . text) <$> comms
                  pure stateChange
                lift $ tellEvent (First <$> headStateE)
              pure ()
      pure ()

filterUtxos :: Address -> WholeUTXO -> WholeUTXO
filterUtxos addr = Map.filter ((== addr) . HT.address)

tellAction :: (EventWriter t [a] m, Reflex t) => Event t a -> m ()
tellAction = tellEvent . fmap (:[])

idleScreen :: (EventWriter t [ClientInput] m, DomBuilder t m) => Text -> m ()
idleScreen name =
  elClass "div" "p-2 flex flex-row" $ do
    (buttonEl, _) <- elClass' "button" "bg-blue-500 hover:bg-blue-400 active:bg-blue-300 text-white font-bold text-xl px-4 py-2 rounded-md" $ text $ "Initialize head as " <> name
    tellAction $ Init 10 <$ domEvent Click buttonEl

initializingScreen ::
  ( EventWriter t [ClientInput] m,
    Requester t (Client m),
    DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    Prerender t m,
    Response (Client m) ~ Either Text,
    Request (Client m) ~ DemoApi
  ) =>
  Text ->
  Address ->
  Behavior t (Maybe Text) ->
  Event t (ServerOutput tx) ->
  m ()
initializingScreen name actorAddress myVKeyB webSocketMessage = do
  elClass "div" "p-2 flex flex-col" $ do
    newUTXOs <- fmapMaybe eitherToMaybe <$> (requestingJs . (DemoApi_GetActorUTXO actorAddress <$) =<< getPostBuild)
    let commitSelection doCommit = do
          (_, currentSet) <-
            runDynamicWriterT $
              runWithReplace (elClass "div" "p-4 bg-gray-800 rounded mb-2" $ text $ "Getting " <> name <> "'s UTXOs...") $
                (tellDyn <=< utxoPicker True) <$> newUTXOs
          tellAction $ fmap (Commit . fromMaybe mempty) $ current currentSet <@ doCommit
    let hasCommitted =
          attachWithMaybe
            ( \mvkey -> \case
                Committed (Party vk) _ -> guard (Just vk == mvkey)
                _ -> Nothing
            )
            myVKeyB
            webSocketMessage

    mdo
      void . runWithReplace (commitSelection doCommit) . ffor hasCommitted $ \() ->
        elClass "div" "text-xl py-4" $ text "Committed, waiting for the others."
      doCommit <- elClass "div" "flex flex-row mt-4" $ do
        -- Until the head is committed starting the head can be aborted:
        tellAction
          . (Hydra.ClientInput.Abort <$)
          <=< buttonClass "bg-gray-400 hover:bg-gray-300 active:bg-gray-200 text-white font-bold text-xl px-4 py-2 rounded-md mr-2"
          $ text "Abort"
        isDisabled <- holdDyn False (True <$ hasCommitted)
        let cls =
              (bool "bg-blue-500 hover:bg-blue-400 active:bg-blue-300" "bg-gray-500 hover:bg-gray-500 active:bg-gray-500 cursor-not-allowed " <$> isDisabled)
                <> " text-white font-bold text-xl px-4 py-2 rounded-md"
        buttonClass cls $ text "Commit"
      pure ()
    pure ()


openScreen ::
  ( EventWriter t [ClientInput] m,
    Requester t (Client m),
    DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    Prerender t m,
    Request (Client m) ~ DemoApi,
    Response (Client m) ~ Either Text
  ) =>
  Text ->
  [Text] ->
  Address ->
  Event t (ServerOutput tx) ->
  m ()
openScreen name actorNames actorAddress webSocketMessage = do
  -- Get your UTxOs on page load and when we observe a transaction
  tellAction . (GetUTxO <$)
    . ( ( void $
            filter
              ( \case
                  TxSeen {} -> True
                  _ -> False
              )
              webSocketMessage
        )
          <>
      )
    =<< getPostBuild
  let updatedUTXOs =
        fmap (filterUtxos actorAddress)
          . mapMaybe
            ( \case
                GetUTxOResponse utxoz -> Just utxoz
                _ -> Nothing
            )
          $ webSocketMessage
  currentUTXOs <- holdDyn mempty updatedUTXOs
  let ifUTXOs yes no = dyn_ (bool yes no <$> fmap Map.null currentUTXOs)
      ifUTXOsDyn yes no = dyn (bool yes no <$> fmap Map.null currentUTXOs)
  mdo
    (_, currentSet) <-
      runDynamicWriterT
        . runWithReplace (elClass "div" "text-white text-2xl" $ text "Getting your UTxOs")
        $ fmap (tellDyn <=< (pure . pure . filterOutFuel)) updatedUTXOs
    _ <- elClass "div" "mb-4 ml-2" $ dyn_ $ utxoPicker False <$> currentSet
    elClass "div" "text-xl mb-8 ml-2" $ ifUTXOs (text "Send Ada to a participant:") (text "No UTXOs for this participant")
    flip ifUTXOs blank $ do
      (recipientDyn, lovelaceDyn) <- elClass "div" "flex ml-2 mb-2" $
        elClass "div" "w-auto flex flex-row rounded bg-gray-800 mb-2 overflow-hidden" $ do
          ie <- elClass "div" "flex flex-col p-2" $ do
            elClass "div" "text-gray-600 text-sm font-semibold" $ text "LOVELACES"
            inputElement $
              def
                & initialAttributes .~ ("class" =: "bg-gray-800 text-2xl font-bold focus:outline-none p-2" <> "type" =: "number")
                & inputElementConfig_initialValue .~ "1000000"
          recipient <- fmap Reflex.Dom.Core.value $
            elClass "div" "flex flex-col p-2" $ do
              elClass "div" "text-gray-600 text-sm font-semibold uppercase" $ text "To"
              -- FIXME: unsafe head, will crash with <= 1 actors
              dropdown
                (head $ filter (/= name) actorNames)
                (pure (Map.filter (/= name) $ Map.fromList (fmap (\n -> (n, n)) actorNames)))
                $ def & dropdownConfig_attributes .~ pure ("class" =: "bg-gray-800 hover:bg-gray-700 active:bg-gray-900 text-gray-100 font-semibold text-xl px-4 py-2 rounded-md m-2")
          pure (recipient, readMaybe . T.unpack <$> _inputElement_value ie)
      elClass "div" "flex" $ do
        signedTxE <-
          requestingJs $
            DemoApi_MkTx name
              <$> current currentSet
              -- NOTE/TODO(skylar): This is just to default to the minimum
              <*> current (fromMaybe 1000000 <$> lovelaceDyn)
              <*> current recipientDyn
              <@ doSend
        tellAction
          . fmap NewTx
          . fmapMaybe eitherToMaybe
          $ signedTxE

    doSend <- elClass "div" "flex flex-row ml-2" $ do
      sendButtonClick <- flip ifUTXOsDyn (pure never) $ do
        buttonClass "bg-green-500 hover:bg-green-400 active:bg-green-200 text-white font-bold text-xl mr-2 px-4 py-2 rounded-md" $ text "Send"
      tellAction
        . (Close <$)
        <=< buttonClass "bg-red-500 hover:bg-red-400 active:bg-red-200 text-white font-bold text-xl px-4 py-2 rounded-md"
        $ text "Close Head"
      switchHold never sendButtonClick
    pure ()
  pure ()

closedScreen ::
  ( MonadFix m,
    MonadIO m,
    MonadIO (Performable m),
    DomBuilder t m,
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m
  ) =>
  UTCTime ->
  m ()
closedScreen fanoutTime = do
  countDownDyn <- clockLossy 1 fanoutTime
  elClass "div" "text-white text-2xl my-4 ml-2" $ do
    text "Fanout time left: "
    dyn_
      ( text . T.pack . show
          . ceiling
          . diffUTCTime fanoutTime
          . _tickInfo_lastUTC
          <$> countDownDyn
      )
    text " seconds"
