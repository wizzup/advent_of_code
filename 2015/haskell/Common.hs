-- | Common utilities
--

module Common
( rpInt
, bothF
, bothV
)
  where

import Data.Char (isDigit)

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as RP

rpInt :: ReadP Int
rpInt = read <$> RP.munch1 isDigit

-- | Apply a value to tuple of functions
-- >>> bothV 1 (succ, pred)
-- (2,0)
--
bothV :: a -> ((->) a b,(->) a c) -> (b,c)
bothV a (f, g) = (f a, g a)

-- | Apply a function on tuple of values
-- >>> bothF succ (0, 1)
-- (1,2)
--
bothF :: (a -> b) -> (a,a) -> (b,b)
bothF f (a,b) = (f a, f b)
