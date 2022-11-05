{-# LANGUAGE TemplateHaskell #-}
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


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Hydra Pay"
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
      elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com") blank
      elAttr "link" ("href"=:"https://fonts.googleapis.com/css2?family=Inria+Sans:wght@300&family=Inter:wght@100;200;300;400;500;600;700;800;900&family=Krona+One&family=Rajdhani:wght@300;400;500;600;700&display=swap" <> "rel"=:"stylesheet") blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src"=:"https://cdn.tailwindcss.com") blank
  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen overflow-hidden flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $ do
      elClass "div" "flex-shrink-0 px-8 py-4 text-xl font-semibold" $
        routeLink (FrontendRoute_Setup :/ ()) $ el "h1" $ text "Hydra Pay"
      elClass "div" "w-full h-full mt-10 flex-grow px-8" $ do
        postBuild <- getPostBuild

        gotAddrs <- fmap switchDyn $ prerender (pure never) $ do
          rec
            jsBuild <- getPostBuild
            result <- getAndDecode $ "/demo-addresses" <$ leftmost [jsBuild, () <$ failedToLoad]
            let
              -- If we failed it is likely the server just hot reloaded in development
              -- and we just try again immediately
              failedToLoad = fmapMaybe (preview _Nothing) result
              addrsRecv = fmapMaybe (preview _Just) result
          pure addrsRecv

        rec
          addrs <- holdDyn (Nothing :: Maybe [Address]) gotAddrs
          nextId <- foldDyn (+) (0 :: Int) $ 1 <$ newTx

          let
            insertNewTx = (current $ Map.insert <$> nextId) <@> newTx

          latestPopupTxs <- foldDyn ($) mempty $ mergeWith (.) $ [ insertNewTx
                                                                 , removeStaleTxs
                                                                 ]
          latestTxs <- foldDyn ($) mempty $ mergeWith (.) $ [ insertNewTx
                                                            ]
          let
            totalTxDisplayTime = 4
            -- We assume the first and second index to be Bob and Alice respectively
            bobAddress = ffor addrs $ join . fmap (^? ix 0)
            aliceAddress = ffor addrs $ join . fmap (^? ix 1)

            newTxPopup tx = do
              transitionIn <- getPostBuild >>= delay 0.1
              transitionOut <- tickLossyFromPostBuildTime totalTxDisplayTime
              removeThisPopup <- tickLossyFromPostBuildTime $ totalTxDisplayTime + 2
              visible <- holdDyn False $ mergeWith (&&) [True <$ transitionIn, False <$ transitionOut]
              let
                mkClasses b =
                  mconcat [ "transition pointer-events-none duration-1000 px-8 py-4 rounded-lg bg-white mb-4 drop-shadow-lg "
                          , bool "opacity-0" "opacity-100" b
                          ]
              elDynClass "div" (mkClasses <$> visible) $ do
                elClass "div" "text-xs text-gray-500 mb-2" $ text "New Transaction"

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
              pure $ () <$ removeThisPopup

          -- When we get a server response, go to open a channel
          newTx <- appView bobAddress aliceAddress latestTxs

          removeStaleTxs <- elClass "div" "absolute top-0 right-0 p-4 h-full overflow-hidden leading-none" $ elClass "div" "flex flex-col-reverse" $ do
            eventList <- listWithKey latestPopupTxs $ \txid tx -> do
              removalEv <- newTxPopup tx
              pure $ Map.delete txid <$ removalEv

            pure $ switchDyn $ mergeWith (.) . Map.elems <$> eventList
        setRoute $ FrontendRoute_OpenChannel :/ () <$ gotAddrs
        setRoute $ FrontendRoute_Setup :/ () <$ postBuild
        setRoute $ FrontendRoute_PaymentChannel :/ () <$ newTx
        pure ()
  }

data DemoTx = DemoTx
  { demoTx_to :: T.Text
  , demoTx_amount :: Lovelace
  , demoTx_time :: Pico
  }
  deriving (Eq, Show)

showAsMs :: Pico -> T.Text
showAsMs = T.pack . printf "%.2f" . (realToFrac :: Pico -> Float) . (*100)

appView ::
  ( Reflex t
  , PostBuild t m
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
  fmap switchDyn $ elClass "div" "text-gray-700 max-w-4xl mx-auto p-4 rounded" $ subRoute $ \case
    FrontendRoute_Setup -> do
      elClass "div" "w-full h-full text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"
        elClass "div" "text-lg" $ text "populating addresses..."
        pure never

    FrontendRoute_OpeningChannel -> do
      elClass "div" "w-full h-full text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"
        elClass "div" "text-lg" $ text "Sending funds from Bob and Alice into hydra pay..."

        prerender_ blank $ do
          postBuild <- getPostBuild
          result :: Event t (Maybe T.Text) <- getAndDecode $ "/demo-fund-init" <$ postBuild
          setRoute $  FrontendRoute_PaymentChannel :/ () <$ result
      pure never

    FrontendRoute_ClosingChannel -> do
      elClass "div" "w-full h-full text-3xl flex flex-col justify-center items-center" $ do
        el "div" $ text "Fast Payments Demo"
        elClass "div" "text-lg" $ text "Settling payment channel on L1..."

        prerender_ blank $ do
          postBuild <- getPostBuild
          result :: Event t (Maybe T.Text) <- getAndDecode $ "/demo-close-fanout" <$ postBuild
          setRoute $  FrontendRoute_OpenChannel :/ () <$ result
      pure never

    FrontendRoute_OpenChannel -> do
      -- Page Title
      elClass "div" "mb-2 font-semibold" $ text "Open a Payment Channel"
      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Balances
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "Current Balances"

      elClass "div" "flex flex-row mt-4" $ do
        elClass "div" "text-lg flex flex-col mr-10" $ do
          elClass "div" "font-semibold" $ text "Bob"
          -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
          elClass "div" "font-semibold" $ dyn_ $ ffor bobAddress $ \case
            Nothing -> pure ()
            Just addr -> prerender_ blank $ do
              addrLoad <- getPostBuild
              gotBalance <- getAndDecode $ "/hydra/l1-balance/" <> addr <$ addrLoad
              mBalance <- holdDyn (Nothing :: Maybe Float) $ fmap lovelaceToAda <$> gotBalance
              dyn_ $ ffor mBalance $ \case
                Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                Just balance -> do
                  text $ T.pack . printf "%.2f" $ balance
                  text " ADA"

        elClass "div" "text-lg flex flex-col mr-10" $ do
          elClass "div" "font-semibold" $ text "Alice"
          -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
          elClass "div" "font-semibold" $ dyn_ $ ffor aliceAddress $ \case
            Nothing -> pure ()
            Just addr -> prerender_ blank $ do
              addrLoad <- getPostBuild
              gotBalance <- getAndDecode $ "/hydra/l1-balance/" <> addr <$ addrLoad
              mBalance <- holdDyn (Nothing :: Maybe Float) $ fmap lovelaceToAda <$> gotBalance
              dyn_ $ ffor mBalance $ \case
                Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                Just balance -> do
                  text $ T.pack . printf "%.2f" $ balance
                  text " ADA"

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
        let
          headBalanceWidget name addr refetch =
            elClass "div" "text-lg flex flex-col mr-10" $ do
              elClass "div" "font-semibold" $ text name
              -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
              elClass "div" "font-semibold" $ dyn_ $ ffor addr $ \case
                Nothing -> pure ()
                Just addr -> prerender_ blank $ mdo
                  addrLoad <- getPostBuild
                  balanceResult :: Event t (Maybe (Either HydraPayError Int)) <- getAndDecode $ "/hydra/head-balance/demo/" <> addr <$ leftmost [addrLoad, reqFailed, () <$ refetch]
                  let
                    gotBalance = fmapMaybe (preview (_Just . _Right)) balanceResult
                    reqFailed = fmapMaybe (preview _Nothing) balanceResult

                  mBalance <- holdDyn (Nothing :: Maybe Float) $ Just . lovelaceToAda <$> gotBalance

                  dyn_ $ ffor mBalance $ \case
                    Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                    Just balance -> do
                      text $ T.pack . printf "%.2f" $ balance
                      text " ADA"

        headBalanceWidget "Bob" bobAddress never
        headBalanceWidget "Alice" aliceAddress never

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

      lovelaceSendAmount :: Dynamic t (Maybe Int) <- el "div" $ elClass "div" "mt-4 w-full border-2 border-gray-200 flex flex-row items-center" $ do
          amountInput <- inputElement $ def
            & initialAttributes .~ ("class" =: "text-gray-500 w-full px-8 py-6 bg-transparent text-center text-xl" <> "placeholder" =: "1 ADA" <> "type" =: "number")
            & inputElementConfig_initialValue .~ "10"
          elClass "span" "mx-2 my-1" $ text "ADA"
          pure $ fmap (round . ada) . readMaybe . T.unpack <$> _inputElement_value amountInput

      (sendButton, _) <- elClass' "button" "rounded mt-4 p-4 text-center w-full bg-gray-800 text-white font-bold" $ text "Send ADA"

      let
        txSendPayload = liftA2 (HeadSubmitTx "demo") <$> toAddr <*> lovelaceSendAmount
        sendAda = fmapMaybe (preview _Just) $ current txSendPayload <@ domEvent Click sendButton
        sendUrl = (fmap ("/hydra/submit-tx/"<>) <$> fromAddr)
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
        elClass "div" "text-lg flex flex-col mr-10" $ do
          elClass "div" "font-semibold" $ text "Bob"
          -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
          elClass "div" "font-semibold" $ dyn_ $ ffor bobAddress $ \case
            Nothing -> pure ()
            Just addr -> prerender_ blank $ do
              addrLoad <- getPostBuild
              gotBalance <- getAndDecode $ "/hydra/l1-balance/" <> addr <$ addrLoad
              mBalance <- holdDyn (Nothing :: Maybe Float) $ fmap lovelaceToAda <$> gotBalance
              dyn_ $ ffor mBalance $ \case
                Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                Just balance -> do
                  text $ T.pack . printf "%.2f" $ balance
                  text " ADA"

        elClass "div" "text-lg flex flex-col mr-10" $ do
          elClass "div" "font-semibold" $ text "Alice"
          -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
          elClass "div" "font-semibold" $ dyn_ $ ffor aliceAddress $ \case
            Nothing -> pure ()
            Just addr -> prerender_ blank $ do
              addrLoad <- getPostBuild
              gotBalance <- getAndDecode $ "/hydra/l1-balance/" <> addr <$ addrLoad
              mBalance <- holdDyn (Nothing :: Maybe Float) $ fmap lovelaceToAda <$> gotBalance
              dyn_ $ ffor mBalance $ \case
                Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                Just balance -> do
                  text $ T.pack . printf "%.2f" $ balance
                  text " ADA"

      -- Divider
      elClass "div" "mt-2 w-full h-px bg-gray-200" blank

      -- Balances
      elClass "div" "text-2xl mb-2 font-semibold mt-8" $ do
        text "Payment Channel Balance"

      rec
        (bobBalance, aliceBalance) <- elClass "div" "flex flex-row mt-4" $ do
          let
            headBalanceWidget name addr refetch =
              elClass "div" "text-lg flex flex-col mr-10" $ do
                elClass "div" "font-semibold" $ text name
                -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
                balanceDynChanged <- (fmap . fmap) join $ elClass "div" "font-semibold" $ dyn $ ffor addr $ \case
                  Nothing -> pure $ constDyn 0
                  Just addr -> prerender (pure $ constDyn 0) $ mdo
                    addrLoad <- getPostBuild
                    balanceResult :: Event t (Maybe (Either HydraPayError Int)) <- getAndDecode $ "/hydra/head-balance/demo/" <> addr <$ leftmost [addrLoad, reqFailed, () <$ refetch]
                    let
                      gotBalance = fmapMaybe (preview (_Just . _Right)) balanceResult
                      reqFailed = fmapMaybe (preview _Nothing) balanceResult

                    mBalance <- holdDyn (Nothing :: Maybe Float) $ Just . lovelaceToAda <$> gotBalance

                    dyn_ $ ffor mBalance $ \case
                      Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                      Just balance -> do
                        text $ T.pack . printf "%.2f" $ balance
                        text " ADA"

                    pure $ maybe 0 id <$> mBalance

                balanceDyn <- join <$> holdDyn (pure 0) balanceDynChanged
                pure $ balanceDyn

          bb <- headBalanceWidget "Bob" bobAddress autoTx
          ab <- headBalanceWidget "Alice" aliceAddress autoTx
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
      
        routeLink (FrontendRoute_SendFunds :/ ()) $ elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold mr-4" $ text "Send ADA"
        (automateButton, _) <- elClass' "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold" $ dynText $ ffor automating $ \case
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

              sendUrl = (fmap ("/hydra/submit-tx/"<>) <$> nextFromAddr)

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

      elClass "div" "flex flex-col-reverse overflow-scroll-y flex-grow" $ dyn_ $ ffor (Map.null <$> latestTxs) $ \case
        True -> elClass "div" "text-gray-500" $ text "There is no history yet for this payment channel"
        False -> do
          _ <- listWithKey latestTxs $ \_ tx -> historyItem tx
          pure ()

      pure autoTx
