{-# LANGUAGE TemplateHaskell #-}
module Paths where
import System.Which (staticWhich)

hydraToolsPath :: FilePath
hydraToolsPath = $(staticWhich "hydra-tools")

realpathPath :: FilePath
realpathPath = $(staticWhich "realpath")

dirnamePath :: FilePath
dirnamePath = $(staticWhich "dirname")

