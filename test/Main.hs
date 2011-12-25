module Main (main) where

import           Test.Framework (defaultMain)
import           Test.Framework.Providers.DocTest

import qualified ParseTest

main = do
  doctests <- docTest ["Text.Format"] ["-i../src"]
  defaultMain [doctests, ParseTest.tests]
