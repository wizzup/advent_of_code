import Control.Monad (guard)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map as M

import Foreign.Marshal.Utils (fromBool) -- True = 1, False = 0

main :: IO ()
main = interact solve

solve, solveA, solveB :: String -> String
solve s = "A: " ++ solveA s ++ "\nB: " ++ solveB s ++ "\n"

solveA = show . computeA . words
solveB = computeB . words

-- |
-- >>> computeA ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
-- 12
--
computeA :: [String] -> Int
computeA xs = countTwo * countThree
  where countTwo   = cal haveTwo xs
        countThree = cal haveThree xs
        cal f = sum . map (fromBool . f . count)

-- |
-- >>> count "hello"
-- fromList [('e',1),('h',1),('l',2),('o',1)]
--
count :: String -> Map Char Int
count xs = foldr f M.empty xs
  where f :: Ord a => a -> Map a Int -> Map a Int
        f c m
          | c `M.member` m = M.adjust succ c m
          | otherwise      = M.insert c 1 m

-- |
-- >>> haveTwo . count $  "hello"
-- True
--
-- >>> haveThree . count $  "heeello"
-- True
--
haveTwo, haveThree :: Map Char Int -> Bool
haveTwo   = any (== 2)
haveThree = any (== 3)

-- |
-- >>> computeB ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
-- "fgij"
--
computeB :: [String] -> String
computeB xs = head found
  where found = do
          i <- xs
          j <- xs
          let o = oneOff i j
          guard . isJust $ o
          return . fromJust $ o

-- |
-- >>> oneOff "fghij" "fguij"
-- Just "fgij"
--
oneOff :: Eq a => [a] -> [a] -> Maybe [a]
oneOff xs ys
  | countDif xs ys /= 1 = Nothing
  | otherwise           = Just $ getDif xs ys

-- |
-- >>> countDif "fghij" "fguij"
-- 1
countDif :: Eq a => [a] -> [a] -> Int
countDif xs ys = sum $ zipWith f xs ys
  where f a b | a == b = 0
              | otherwise = 1

-- |
-- >>> getDif "fghij" "fguij"
-- "fgij"
--
getDif :: Eq a => [a] -> [a] -> [a]
getDif []     [] = []
getDif (x:xs) (y:ys)
  | x == y = x : getDif xs ys
  | otherwise = getDif xs ys
