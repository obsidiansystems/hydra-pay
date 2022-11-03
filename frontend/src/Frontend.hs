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

import Hydra.Types
import HydraPay

import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Text as T

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static

import Reflex.Dom.Core
import Common.Route

import Control.Lens
import Control.Monad
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
  , _frontend_body = elAttr "div" ("class" =: "w-screen h-screen flex flex-col bg-gray-100" <> "style" =: "font-family: 'Inter', sans-serif;") $ do
      elClass "div" "flex-shrink-0 px-8 py-4 text-xl font-semibold" $
        routeLink (FrontendRoute_Setup :/ ()) $ el "h1" $ text "Hydra Pay"
      elClass "div" "w-full h-full mt-10 flex-grow px-8" $ do
        postBuild <- getPostBuild

        gotAddrs <- fmap switchDyn $ prerender (pure never) $ mdo
          jsBuild <- getPostBuild
          result <- getAndDecode $ "/demo-addresses" <$ leftmost [jsBuild, () <$ failedToLoad]
          let
            -- If we failed it is likely the server just hot reloaded in development
            -- and we just try again immediately
            failedToLoad = fmapMaybe (preview _Nothing) result
            addrsRecv = fmapMaybe (preview _Just) result
          pure addrsRecv

        addrs <- holdDyn (Nothing :: Maybe [Address]) gotAddrs

        let
          -- We assume the first and second index to be Bob and Alice respectively
          bobAddress = ffor addrs $ join . fmap (^? ix 0)
          aliceAddress = ffor addrs $ join . fmap (^? ix 1)

        -- When we get a server response, go to open a channel
        setRoute $ FrontendRoute_OpenChannel :/ () <$ gotAddrs
        setRoute $ FrontendRoute_Setup :/ () <$ postBuild

        elClass "div" "text-gray-700 max-w-4xl mx-auto p-4 rounded" $ subRoute_ $ \case
          FrontendRoute_Setup -> do
            elClass "div" "w-full h-full text-3xl flex flex-col justify-center items-center" $ do
              el "div" $ text "Fast Payments Demo"
              elClass "div" "text-lg" $ text "populating addresses..."

          FrontendRoute_OpeningChannel -> do
            elClass "div" "w-full h-full text-3xl flex flex-col justify-center items-center" $ do
              el "div" $ text "Fast Payments Demo"
              elClass "div" "text-lg" $ text "Sending funds from Bob and Alice into hydra pay..."

              prerender_ blank $ do
                postBuild <- getPostBuild
                result :: Event t (Maybe T.Text) <- getAndDecode $ "/demo-fund-init" <$ postBuild
                setRoute $  FrontendRoute_PaymentChannel :/ () <$ result

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

          FrontendRoute_SendFunds -> mdo
            -- Page Header
            elClass "div" "w-full flex flex-row items-baseline justify-between" $ do
              elClass "div" "mb-2 font-semibold" $ text "Bob & Alice's Payment Channel"

            -- Divider
            elClass "div" "mt-2 w-full h-px bg-gray-200" blank

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

              headBalanceWidget "Bob" bobAddress sendAdaResponse
              headBalanceWidget "Alice" aliceAddress sendAdaResponse

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
                  elClass "div" "font-semibold" $ dynText $ ffor bobIsTo $ \case
                    True -> "Bob"
                    False -> "Alice"

              let
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

            sendAdaResponse <- fmap switchDyn $ prerender (pure never) $ do
              performRequestAsync $ fmapMaybe (preview _Just) $ current sendReq <@ sendAda
            pure ()

          FrontendRoute_PaymentChannel -> do
            -- Page Title
            elClass "div" "w-full flex flex-row items-baseline justify-between" $ do
              elClass "div" "mb-2 font-semibold" $ text "Bob & Alice's Payment Channel"
              routeLink (FrontendRoute_OpenChannel :/ ()) $ elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold" $ text "Close Payment Channel"

            -- Divider
            elClass "div" "mt-2 w-full h-px bg-gray-200" blank

            -- Balances
            elClass "div" "text-2xl mb-2 font-semibold mt-8" $ do
              text "Balances "
              elClass "span" "text-sm" $ text "In payment channel"

            elClass "div" "flex flex-row mt-4" $ do
              let
                headBalanceWidget name addr =
                  elClass "div" "text-lg flex flex-col mr-10" $ do
                    elClass "div" "font-semibold" $ text name
                    -- NOTE(skylar): We assume we have loaded bobAddress if this is visible, so we don't worry about the outer Nothing
                    elClass "div" "font-semibold" $ dyn_ $ ffor addr $ \case
                      Nothing -> pure ()
                      Just addr -> prerender_ blank $ mdo
                        addrLoad <- getPostBuild
                        balanceResult :: Event t (Maybe (Either HydraPayError Int)) <- getAndDecode $ "/hydra/head-balance/demo/" <> addr <$ leftmost [addrLoad, reqFailed]
                        let
                          gotBalance = fmapMaybe (preview (_Just . _Right)) balanceResult
                          reqFailed = fmapMaybe (preview _Nothing) balanceResult

                        mBalance <- holdDyn (Nothing :: Maybe Float) $ Just . lovelaceToAda <$> gotBalance

                        dyn_ $ ffor mBalance $ \case
                          Nothing -> elClass "div" "animate-pulse bg-gray-700 w-16 h-4" blank
                          Just balance -> do
                            text $ T.pack . printf "%.2f" $ balance
                            text " ADA"

              headBalanceWidget "Bob" bobAddress
              headBalanceWidget "Alice" aliceAddress

            -- Divider
            elClass "div" "mt-2 w-full h-px bg-gray-200" blank

            -- Statistics
            elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "Statistics"

            elClass "div" "flex flex-row mt-4" $ do
              elClass "div" "mr-16" $ do
                elClass "div" "text-lg font-semibold" $ text "Total Transactions"
                elClass "div" "text-3xl text-gray-600" $ text "0"

              elClass "div" "mr-16" $ do
                elClass "div" "text-lg font-semibold" $ text "Total Time"
                elClass "div" "text-3xl text-gray-600" $ text "0ms"

              elClass "div" "" $ do
                elClass "div" "text-lg font-semibold" $ text "Total Fees"
                elClass "div" "text-3xl text-gray-600" $ do
                  text "0"
                  elClass "span" "text-xl text-gray-600 font-semibold" $ text "ADA"

            -- Divider
            elClass "div" "mt-2 w-full h-px bg-gray-200" blank

            -- Action buttons: Send & Automate
            routeLink (FrontendRoute_SendFunds :/ ()) $ elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold mr-4" $ text "Send ADA"
            elClass "button" "rounded mt-4 px-6 py-2 text-center bg-gray-800 text-white font-semibold" $ text "Automate"

            -- Transaction History
            elClass "div" "text-2xl mb-2 font-semibold mt-8" $ text "History"

            elClass "div" "text-gray-500" $ text "There is no history yet for this payment channel"

      pure ()
  }
