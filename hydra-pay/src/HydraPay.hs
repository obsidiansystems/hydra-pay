{-# LANGUAGE TemplateHaskell #-}

module HydraPay where

import System.Which
import System.Process

cardanoNodePath :: FilePath
cardanoNodePath = $(staticWhich "cardano-node")

cardanoCliPath :: FilePath
cardanoCliPath = $(staticWhich "cardano-cli")

hydraNodePath :: FilePath
hydraNodePath = $(staticWhich "hydra-node")

runHydraPay :: IO a -> IO a
runHydraPay action = do
  withCreateProcess _ (\_ _ _ _ _ -> action)
