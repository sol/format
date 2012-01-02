{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Spec (main) where

import           Test.Hspec.ShouldBe
import           Language.Haskell.TH
import           Control.Monad
import           Control.Applicative

import           Text.Format
import qualified Text.Format.Formattable as Formattable

actual `shouldBeQ` expected = join (shouldBe <$> runQ actual <*> runQ expected)

main = hspec $ do

  describe "$format" $ do
    it "interpolates identifiers in strings" $ do
      let name = "John"
      $(format "Hello, {name}!") `shouldBe` "Hello, John!"

    it "produces { from {{" $ do
      $(format "Hello, {{name}!") `shouldBe` "Hello, {name}!"

  describe "formatS" $ do

    it "produces no redundancy on empty string" $ do
      formatS "" `shouldBeQ` [|id|]

    it "produces no redundancy on a single string literal" $ do
      formatS "foo" `shouldBeQ` [|showString "foo"|]

    it "produces no redundancy on a single capture" $ do
      let x = "foo"
      formatS "{x}" `shouldBeQ` [|Formattable.formatS $(dyn "x")|]
