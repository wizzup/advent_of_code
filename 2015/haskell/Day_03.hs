module Day_03
( partOne
, partTwo
) where

import Control.Arrow ((***))
import Data.Set (Set)
import qualified Data.Set as S

import Common (bothF, bothV)

partOne :: IO Int
partOne = process stepOne

partTwo :: IO Int
partTwo = process stepTwo

process :: (String -> Int) -> IO Int
process f = f <$> readFile "../input/03.txt"

stepOne :: String -> Int
stepOne = S.size . moveSet

stepTwo :: String -> Int
stepTwo xs = S.size $ a `S.union` b
  where (a,b) = moveSet `bothF` splitOddEven xs

type Position = (Int,Int)

stepMove :: Char -> Position -> Position
stepMove '^' (x,y) = (x, succ y)
stepMove 'v' (x,y) = (x, pred y)
stepMove '>' (x,y) = (succ x, y)
stepMove '<' (x,y) = (pred x, y)
stepMove c   _     = error $ "stepMove: unknown direction " ++ show c

-- |
-- >>> S.size $ moveSet ">"
-- 2
-- >>> S.size $ moveSet "^>v<"
-- 4
-- >>> S.size $ moveSet "^v^v^v^v^v"
-- 2

moveSet :: String -> Set Position
moveSet xs = S.singleton (0,0) `S.union` a `S.union` b
  where (a,b) = (***) id S.singleton $ moveSet' xs
        moveSet':: String -> (Set Position, Position)
        moveSet' = foldl f (S.empty, (0,0))
        f :: (Set Position, Position) -> Char -> (Set Position, Position)
        f (s,p) c = stepMove c p `bothV` (S.union s . S.singleton, id)

-- | split a list into two list by its index number
-- https://stackoverflow.com/a/36058429/1664572
-- >>> splitOddEven [1..10]
-- ([1,3,5,7,9],[2,4,6,8,10])
--
splitOddEven :: [a] -> ([a],[a])
splitOddEven = foldr (\x ~(y2,y1) -> (x:y1, y2)) ([],[])
