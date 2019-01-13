module Day_02
( partOne
, partTwo
) where

partOne :: IO Int
partOne = sum . map (diff1 . minMax) <$> getInputs

partTwo :: IO Int
partTwo = sum . map (diff2 . divPair) <$> getInputs

readMultiLine :: FilePath -> IO [String]
readMultiLine fp = lines <$> readFile fp

getInputs :: IO [[Int]]
getInputs = map (map read . words) <$> readMultiLine "../input/02.txt"

-- |
-- >>> minMax [1..100]
-- (1,100)
--
minMax :: [Int] -> (Int,Int)
minMax [] = error "minMax: empty list"
minMax xs = foldr f (maxBound,minBound) xs
  where f :: Int -> (Int,Int) -> (Int,Int)
        f n (a,b) = (min n a, max n b)

diff1 :: (Int, Int) -> Int
diff1 = uncurry $ flip (-)

-- |
-- >>> divPair [5,9,2,8]
-- (8,2)
-- >>> divPair [9,4,7,3]
-- (9,3)
-- >>> divPair [3,8,6,5]
-- (6,3)
--
divPair :: [Int] -> (Int,Int)
divPair [] = error "divPair: empty list"
divPair xs = head [(a,b) | a <- xs, b <- xs, a `mod` b == 0, a /= b]

diff2 :: (Int,Int) -> Int
diff2 = uncurry div
