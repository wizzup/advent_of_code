module Day_X
( partOne
, partTwo
) where

partOne :: IO Int
partOne = process stepOne

partTwo :: IO Int
partTwo = process stepTwo

process :: (String -> Int) -> IO Int
process f = f <$> readFile "../input/X.txt"

stepOne :: String -> Int
stepOne _ = 0

stepTwo :: String -> Int
stepTwo _ = 1
