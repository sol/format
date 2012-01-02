module Main (main) where

import           Test.Framework (defaultMain)
import           Test.Framework.Providers.DocTest

main = do
  doctests <- docTest ["Text.Format"] ["-i../src"]
  defaultMain [doctests]
