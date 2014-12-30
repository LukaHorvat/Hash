module Parse.Common where

import Prelude ()
import Text.Parsec.Char
import Text.Parsec.Text
import ClassyPrelude

text :: String -> Parser Text
text str = pack <$> string str