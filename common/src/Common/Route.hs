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
  BackendRoute_DemoFundInit :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data HydraPayRoute :: * -> * where
  HydraPayRoute_Head :: HydraPayRoute ()
  HydraPayRoute_HeadStatus :: HydraPayRoute Text
  -- | This route will give you a transaction that you can use to add funds to the proxy of a address
  HydraPayRoute_AddFundsTx :: HydraPayRoute (Text :. Int)
  HydraPayRoute_AddFuelTx :: HydraPayRoute (Text :. Int)
  HydraPayRoute_Withdraw :: HydraPayRoute ()
  HydraPayRoute_Init :: HydraPayRoute ()
  HydraPayRoute_Commit :: HydraPayRoute ()
  HydraPayRoute_Close :: HydraPayRoute Text
  HydraPayRoute_SubmitTx :: HydraPayRoute Text
  HydraPayRoute_HeadBalance :: HydraPayRoute (Text :. Text)
  HydraPayRoute_L1Balance :: HydraPayRoute Text

hydraPayRouteEncoder ::( MonadError Text check
                       , MonadError Text parse
                       )
                     => Encoder check parse (R HydraPayRoute) PageName
hydraPayRouteEncoder = pathComponentEncoder $ \case
  HydraPayRoute_Head -> PathSegment "heads" $ unitEncoder mempty
  HydraPayRoute_HeadStatus -> PathSegment "head" singlePathSegmentEncoder
  HydraPayRoute_AddFundsTx -> PathSegment "add-funds" $ pathParamEncoder id $ singlePathSegmentEncoder . unsafeTshowEncoder
  HydraPayRoute_AddFuelTx -> PathSegment "add-fuel" $ pathParamEncoder id $ singlePathSegmentEncoder . unsafeTshowEncoder
  HydraPayRoute_Init -> PathSegment "init" $ unitEncoder mempty
  HydraPayRoute_Commit -> PathSegment "commit" $ unitEncoder mempty
  HydraPayRoute_Withdraw -> PathSegment "withdraw" $ unitEncoder mempty
  HydraPayRoute_Close -> PathSegment "close" singlePathSegmentEncoder
  HydraPayRoute_SubmitTx -> PathSegment "submit-tx" singlePathSegmentEncoder
  HydraPayRoute_HeadBalance -> PathSegment "head-balance" $ pathParamEncoder id $ singlePathSegmentEncoder
  HydraPayRoute_L1Balance -> PathSegment "l1-balance" singlePathSegmentEncoder

data FrontendRoute :: * -> * where
  FrontendRoute_Setup :: FrontendRoute ()
  FrontendRoute_OpenChannel :: FrontendRoute ()
  FrontendRoute_PaymentChannel :: FrontendRoute ()
  FrontendRoute_OpeningChannel :: FrontendRoute ()
  FrontendRoute_SendFunds :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" $ unitEncoder mempty
      BackendRoute_DemoAddresses -> PathSegment "demo-addresses" $ unitEncoder mempty
      BackendRoute_DemoFundInit -> PathSegment "demo-fund-init" $ unitEncoder mempty
      BackendRoute_HydraPay -> PathSegment "hydra" hydraPayRouteEncoder
  )
  (\case
      FrontendRoute_Setup -> PathEnd $ unitEncoder mempty
      FrontendRoute_OpenChannel -> PathSegment "open" $ unitEncoder mempty
      FrontendRoute_OpeningChannel -> PathSegment "opening" $ unitEncoder mempty
      FrontendRoute_PaymentChannel -> PathSegment "channel" $ unitEncoder mempty
      FrontendRoute_SendFunds -> PathSegment "send" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''HydraPayRoute
  ]
