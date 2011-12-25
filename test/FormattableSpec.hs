{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module FormattableSpec (main) where

import           Test.Hspec.ShouldBe

import           Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)

import           Text.Format.Formattable

data Foo = Foo deriving Show

main = hspec $ do
  describe "Text.Formattable.formatS" $ do
    it "works for String" $ do
      format ("some string" :: String) `shouldBe` "some string"

    it "works for Data.Text" $ do
      format ("some text" :: Text) `shouldBe` "some text"

    it "works for Data.Text.Lazy" $ do
      format ("some lazy text" :: L.Text) `shouldBe` "some lazy text"

    it "works for instances of Show" $ do
      format Foo `shouldBe` "Foo"

    it "works for Int" $ do
      format (10 :: Int) `shouldBe` "10"

    it "works for instances of Num" $ do
      formatNum (23 :: Float) `shouldBe` "23.0"

  where
    format :: Formattable a => a -> String
    format x = formatS x ""

    formatNum :: Num a => a -> String
    formatNum x = format x
