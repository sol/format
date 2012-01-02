{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Spec (main) where

import Test.Hspec.ShouldBe

import Text.Format (format)

main = hspec $ do

  describe "format" $ do
    it "interpolates identifiers in strings" $ do
      let name = "John"
      $(format "Hello, {name}!") `shouldBe` "Hello, John!"

    it "produces { from {{" $ do
      $(format "Hello, {{name}!") `shouldBe` "Hello, {name}!"
