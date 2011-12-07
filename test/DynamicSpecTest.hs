{-# LANGUAGE TemplateHaskell #-}
module DynamicSpecTest (tests) where
import Test.Framework.TH (testGroupGenerator)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit
import Test.QuickCheck

import DynamicSpec

import Text.ParserCombinators.ReadP

tests = $(testGroupGenerator)

escapeLiteral :: String -> String
escapeLiteral        ""  =             ""
escapeLiteral ('$' : xs) = '$' : '$' : escapeLiteral xs
escapeLiteral ( x  : xs) =        x  : escapeLiteral xs

instance Arbitrary Literal where
  arbitrary = do
    s <- arbitrary `suchThat` (not . elem '{')
    return $ Literal $ escapeLiteral s

instance Arbitrary Capture where
  arbitrary = do
    s <- arbitrary `suchThat` (not . null) `suchThat` (not . elem '}')
    return (Capture s)

prop_parse t = (parse . render) t == Just t

render (Literal s, x) = escapeLiteral s ++ render_ x
  where
    render_ [] = ""
    render_ ((Capture c, Literal s) : xs) = concat ["${", c, "}", escapeLiteral s, render_ xs]
