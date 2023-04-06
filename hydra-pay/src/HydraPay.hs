{-# LANGUAGE TemplateHaskell #-}

module HydraPay where

import Data.Int

import System.IO
import System.Exit
import System.Which
import System.Process

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Logger.Extras
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.Aeson as Aeson

import HydraPay.Cardano.Cli
import HydraPay.Cardano.Node

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

runHydraPay :: NodeConfig -> (NodeInfo -> IO a) -> IO a
runHydraPay = withCardanoNode
