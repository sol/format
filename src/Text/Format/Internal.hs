{-# LANGUAGE TemplateHaskell #-}
module Text.Format.Internal (format, formatS, parse, Node(..)) where

import           Language.Haskell.TH
import           Control.Monad (liftM2)
import           Text.ParserCombinators.ReadP
import qualified Text.Format.Formattable as Formattable

-- | Format a string.
--
-- Identifiers in curly brackets are interpolated from the outer scope
-- (dynamically scoped), and must be instances of `Formattable.Formattable`.
--
-- Example:
--
-- >>> :set -fth
-- >>> let x = "bar"
-- >>> let y = 23
-- >>> $(format "foo {x} {y} baz")
-- "foo bar 23 baz"
format :: String -> Q Exp
format s = [| $(formatS s) "" |]

-- | `ShowS` version of `format`
formatS :: String -> Q Exp
formatS s =
  case parse s of
    Nothing -> fail ("Invalid format string: " ++ show s)
    Just xs -> go xs
  where
    -- We make some effort here to produces code, that contains no
    -- redundancies.  This is a good idea, as the code makes it into error
    -- messages.
    go [] = [|id|]
    go [Capture x] = [|Formattable.formatS $(dyn x)|]
    go [Literal x] = [|showString x|]
    go (Capture x : xs) = [|Formattable.formatS $(dyn x) . $(go xs)|]
    go (Literal x : xs) = [|showString x . $(go xs)|]

data Node = Literal String | Capture String

-- | Parse a format strig.
parse :: String -> Maybe [Node]
parse s =
  case readP_to_S nodes s of
    [(x, "")] -> Just x
    _         -> Nothing

nodes :: ReadP [Node]
nodes = many (capture <++ literal) << eof

node :: ReadP Node
node = capture <++ literal

literal :: ReadP Node
literal = Literal `fmap` go
  where
    go = do
      x  <- (string "{{" >> return '{') <++ satisfy (/='{')
      xs <- munch (/= '{')
      ys <- go <++ return ""
      return $ (x:xs) ++ ys

capture :: ReadP Node
capture = Capture `fmap` (char '{' >> name << char '}')
  where
    name = munch1 (\c -> c /= '{' && c /= '}')

-- | same as <*
(<<) :: Monad m => m a -> m b -> m a
(<<) = liftM2 const
