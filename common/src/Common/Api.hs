{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Api where

import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras (Has)
import Data.Constraint.Extras.TH (deriveArgDict)

data DemoApi :: * -> * where
  DemoApi_GetWholeUTXO :: DemoApi T.Text

deriveJSONGADT ''DemoApi
deriveArgDict ''DemoApi

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"
