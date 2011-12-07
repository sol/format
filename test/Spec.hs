{-# LANGUAGE TemplateHaskell #-}
module Spec (main) where

import TestUtil

import Text.Format (format)

main = hspec $ do

  describe "format" $ do
    it "interpolates identifiers in strings" $ do
      let name = "John"
      $(format "Hello, ${name}!") `shouldBe` "Hello, John!"

    it "produces a dollar sign from $$" $ do
      $(format "Hello, $${name}!") `shouldBe` "Hello, ${name}!"
