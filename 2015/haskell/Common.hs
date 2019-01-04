module Common
( rpInt
)
  where

import Data.Char (isDigit)

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as RP

rpInt :: ReadP Int
rpInt = read <$> RP.munch1 isDigit
