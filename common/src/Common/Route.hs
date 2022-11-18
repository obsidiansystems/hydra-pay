{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route where

-- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

import Control.Monad.Error

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute ()
  BackendRoute_HydraPay :: BackendRoute (R HydraPayRoute)

  BackendRoute_DemoAddresses :: BackendRoute ()
  BackendRoute_DemoTestWithdrawal :: BackendRoute ()
  BackendRoute_DemoApi :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data HydraPayRoute :: * -> * where
  HydraPayRoute_Api :: HydraPayRoute ()

hydraPayRouteEncoder ::( MonadError Text check
                       , MonadError Text parse
                       )
                     => Encoder check parse (R HydraPayRoute) PageName
hydraPayRouteEncoder = pathComponentEncoder $ \case
  HydraPayRoute_Api -> PathSegment "api" $ unitEncoder mempty

data FrontendRoute :: * -> * where
  -- This is for managing Hydra Pay
  FrontendRoute_Monitor :: FrontendRoute ()

  -- These are for the demo
  FrontendRoute_Setup :: FrontendRoute ()
  FrontendRoute_OpenChannel :: FrontendRoute ()
  FrontendRoute_PaymentChannel :: FrontendRoute ()
  FrontendRoute_OpeningChannel :: FrontendRoute ()
  FrontendRoute_SendFunds :: FrontendRoute ()
  FrontendRoute_ClosingChannel :: FrontendRoute ()

  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" $ unitEncoder mempty
      BackendRoute_DemoAddresses -> PathSegment "demo-addresses" $ unitEncoder mempty
      BackendRoute_HydraPay -> PathSegment "hydra" hydraPayRouteEncoder
      BackendRoute_DemoTestWithdrawal -> PathSegment "demo-test" $ unitEncoder mempty
      BackendRoute_DemoApi -> PathSegment "demo-api" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Monitor -> PathEnd $ unitEncoder mempty
      FrontendRoute_Setup -> PathSegment "setup" $ unitEncoder mempty
      FrontendRoute_OpenChannel -> PathSegment "open" $ unitEncoder mempty
      FrontendRoute_OpeningChannel -> PathSegment "opening" $ unitEncoder mempty
      FrontendRoute_PaymentChannel -> PathSegment "channel" $ unitEncoder mempty
      FrontendRoute_SendFunds -> PathSegment "send" $ unitEncoder mempty
      FrontendRoute_ClosingChannel -> PathSegment "closing" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''HydraPayRoute
  ]
