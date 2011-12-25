{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module Text.Format.Formattable where

import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L

class Formattable a where
  formatS :: a -> ShowS

instance Formattable String where
  formatS = showString

instance Formattable Text where
  formatS = formatS . Text.unpack

instance Formattable L.Text where
  formatS = formatS . L.unpack

instance (Show a) => Formattable a where
  formatS = shows
