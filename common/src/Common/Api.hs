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
import qualified Data.Map as Map
import Data.Text (Text)


-- | Friendly name for a Hydra node.
type DemoNodeName = Text

-- | WebSocket URL
type ApiUrl = Text

type RunningNodes = Map DemoNodeName ( Address -- Cardano address
                                     , ApiUrl
                                     )

type HydraDemo =  Map
                  DemoNodeName
                  ( Lovelace -- Seed for actor
                  , Lovelace -- Seed for fuel
                  )

-- FIXME: Api mixes actor names and addresses, use one or the other.
data DemoApi :: * -> * where
  DemoApi_GetActorUTXO :: Address -> DemoApi WholeUTXO
  DemoApi_MkTx
    :: DemoNodeName -- From
    -> WholeUTXO
    -> Lovelace
    -> DemoNodeName -- To
    -> DemoApi Text
  DemoApi_Start :: HydraDemo -> DemoApi RunningNodes

deriveJSONGADT ''DemoApi
deriveArgDict ''DemoApi

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"
