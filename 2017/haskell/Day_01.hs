{-# LANGUAGE ExplicitForAll #-}

module Day1 where

import Data.List.Extra (groupOn)

readSingleLine :: FilePath -> IO String
readSingleLine fp = head . lines <$> readFile fp

getInputs :: IO [Int]
getInputs = map (read . (:[])) <$> readSingleLine "../input/01.txt"

--------------------------------------------------------------------------------
partOne :: IO Int
partOne = checkOne <$> getInputs

-- 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
-- 1111 produces 4 because each digit (all 1) matches the next.
-- 1234 produces 0 because no digit matches the next.
-- 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

duplicates :: forall a. Eq a => [a] -> [(a,Int)]
duplicates = foldr f []
  where f :: Eq a => a -> [(a,Int)] -> [(a,Int)]
        f x [] = [(x,0)]
        f x ((y,n):ys) | x == y    = (y,succ n): f y ys
                       | otherwise = (x, 0) : f y ys

checkOne :: [Int] -> Int
checkOne [] = 0
checkOne [_] = 0
checkOne (x:xs) = foldr f 0 ls
  where f :: (Int,Int) -> Int -> Int
        f (a,b) n = a * b + n
        ls = filter ((> 0). snd) . map head . groupOn fst $ duplicates (x:xs ++ [x])

--------------------------------------------------------------------------------
partTwo :: IO Int
partTwo = checkTwo <$> getInputs


-- 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
-- 1221 produces 0, because every comparison is between a 1 and a 2.
-- 123425 produces 4, because both 2s match each other, but no other digit has a match.
-- 123123 produces 12.
-- 12131415 produces 4.

checkTwo :: [Int] -> Int
checkTwo xs = (* 2) . sum . uncurry (zipWith f) $ splitAt l xs
  where l = length xs `div` 2
        f :: Int -> Int -> Int
        f a b | a == b    = a
              | otherwise = 0
