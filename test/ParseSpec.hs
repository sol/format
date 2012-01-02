{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module ParseSpec (main) where

import Test.Hspec.ShouldBe
import Test.QuickCheck hiding (property)
import Control.Applicative

import qualified Data.Text as Text

import Text.Format.Internal

deriving instance Eq Node
deriving instance Show Node

newtype Nodes = Nodes [Node]
  deriving Show

instance Arbitrary Nodes where
  arbitrary = resize 10 $ do
    n <- arbitrary
    Nodes <$> oneof [sequence (take n startWithLiteral), sequence (take n startWithCapture)]
    where
      startWithLiteral = (concat . repeat) [arbitraryLiteral, arbitraryCapture]
      startWithCapture = (concat . repeat) [arbitraryCapture, arbitraryLiteral]
      arbitraryLiteral = Literal <$> arbitrary `suchThat` (not . null)
      arbitraryCapture = Capture <$> arbitrary `suchThat` (not . null)
                                               `suchThat` (not . elem '{')
                                               `suchThat` (not . elem '}')

render :: [Node] -> String
render = foldr ((++) . f) ""
  where
    f (Literal x) = escapeLiteral x
    f (Capture x) = "{" ++ x ++ "}"

escapeLiteral :: String -> String
escapeLiteral = Text.unpack . Text.replace "{" "{{" . Text.pack

main = hspec $ do
  describe "parse" $ do
    it "parses an empty format string" $ do
      parse "" `shouldBe` Just []

    it "parses a literal" $ do
      parse "foo" `shouldBe` Just [Literal "foo"]

    it "parses a literal with an escaped {" $ do
      parse "{{foo}" `shouldBe` Just [Literal "{foo}"]

    it "parses a capture" $ do
      parse "{foo}" `shouldBe` Just [Capture "foo"]

    it "parses arbitrary literals" $ property $
      \s -> parse (escapeLiteral s) == if null s then Just [] else Just [Literal s]

    it "parses arbitrary format strings" $ property $
      \(Nodes t) -> (parse . render) t == Just t
