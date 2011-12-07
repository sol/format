{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Spec (main, spec) where

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.HUnit
import           Control.Monad

import Text.Format

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = unless (actual == expected) (assertFailure message)
  where
    message = show actual ++ " was not equal to " ++ show expected

main = hspec spec

spec = do

  describe "format" $ do
    it "interpolates identifiers in strings" $ do
      let name = "John"
      $(format "Hello, ${name}!") `shouldBe` "Hello, John!"

    it "produces a dollar sign from $$" $ do
      $(format "Hello, $${name}!") `shouldBe` "Hello, ${name}!"
