{-# LANGUAGE TemplateHaskell #-}

module Common.Api where

import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras (Has)
import Data.Constraint.Extras.TH (deriveArgDict)
import GHC.Generics
import Data.Map (Map)

import Hydra.Types

data DemoApi :: * -> * where
  DemoApi_GetWholeUTXO :: DemoApi WholeUTXO

deriveJSONGADT ''DemoApi
deriveArgDict ''DemoApi

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"
