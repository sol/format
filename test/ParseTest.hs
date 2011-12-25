{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module ParseTest (tests) where

import           Control.Applicative
import qualified Data.Text as Text

import           Test.Framework.TH
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           Text.Format.Internal (parse, Literal(..), Capture(..))

tests = $(testGroupGenerator)
main  = $(defaultMainGenerator)

instance Arbitrary Literal where
  arbitrary = Literal <$> arbitrary

instance Arbitrary Capture where
  arbitrary = Capture <$> arbitrary `suchThat` (not . null)
                                    `suchThat` (not . elem '{')
                                    `suchThat` (not . elem '}')

prop_parse t = (parse . render) t == Just t

render (Literal s, xs) = escapeLiteral s ++ renderList xs
  where
    escapeLiteral = Text.unpack . Text.replace "{" "{{" . Text.pack

    renderList [] = ""
    renderList ((Capture c, Literal y) : ys) = "{" ++ c ++ "}" ++ escapeLiteral y ++ renderList ys
