module Day_X
( partOne
, partTwo
) where

partOne :: IO Int
partOne = process stepOne

partTwo :: IO Int
partTwo = process stepTwo

process :: (String -> Int) -> IO Int
process f = combine . map f . lines <$> readFile "../input/X.txt"

combine :: [Int] -> Int
combine = sum

stepOne :: String -> Int
stepOne _ = 0

stepTwo :: String -> Int
stepTwo _ = 1
