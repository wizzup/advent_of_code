module Day_01
( partOne
, partTwo
) where

partOne :: IO Int
partOne = process countP

partTwo :: IO Int
partTwo = process countF

process :: (String -> Int) -> IO Int
process f = f <$> readFile "../input/01.txt"

step :: Char -> Int -> Int
step '(' = succ
step ')' = pred
step _   = id

-- |
-- >>> countP "(())"
-- 0
-- >>> countP "()()"
-- 0
-- >>> countP "))((((("
-- 3
-- >>> countP "())"
-- -1
-- >>> countP "))("
-- -1
-- >>> countP ")))"
-- -3
-- >>> countP ")())())"
-- -3

countP :: String -> Int
countP = foldr step 0

-- |
-- >>> countF ")"
-- 1
-- >>> countF "()())"
-- 5

countF :: String -> Int
countF = length . takeWhile (/= -1) . scanl (flip step) 0
