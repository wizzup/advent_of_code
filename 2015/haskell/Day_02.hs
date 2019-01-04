module Day_02
( partOne
, partTwo
) where

import           Data.List (sort)
import           Text.ParserCombinators.ReadP (ReadP)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (readPrec)
import qualified Text.ParserCombinators.ReadP as RP

import Common (rpInt)

partOne :: IO Int
partOne = process $ paperSize . read

partTwo :: IO Int
partTwo = process $ ribbon . read

process :: (String -> Int) -> IO Int
process f = sum . map f . lines <$> readFile "../input/02.txt"

data Box = Box Int Int Int
  deriving Show

-- |
-- >>> read "2x3x4" :: Box
-- Box 2 3 4
--
instance Read Box where
  readPrec = lift rpBox

rpBox :: ReadP Box
rpBox = do
  l <- rpInt
  _ <- RP.char 'x'
  w <- rpInt
  _ <- RP.char 'x'
  Box l w <$> rpInt

-- |
-- >>> paperSize $ read "2x3x4"
-- 58
-- >>> paperSize $ read "1x1x10"
-- 43
--
paperSize :: Box -> Int
paperSize = (+) <$> slack <*> area

slack :: Box -> Int
slack (Box l w h) = i * j
  where [i,j,_] = sort [l,w,h]

-- |
-- >>> area $ read "2x3x4"
-- 52
-- >>> area $ read "1x1x10"
-- 42
--
area :: Box -> Int
area (Box l w h) = 2*l*w + 2*w*h + 2*h*l

-- |
-- >>> ribbon $ read "2x3x4"
-- 34
-- >>> ribbon $ read "1x1x10"
-- 14
--
ribbon :: Box -> Int
ribbon (Box l w h) = (i + i + j + j) + (i * j * k)
  where [i,j,k] = sort [l,w,h]
