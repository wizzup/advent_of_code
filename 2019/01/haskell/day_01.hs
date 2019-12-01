---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/1
---------------------------------------------------------------------

-- $
-- >>> fuel <$> [12,14,1969,100756]
-- [2,2,654,33583]
fuel :: Int -> Int
fuel i = (i `div` 3) - 2

part_1 :: [Int] -> Int
part_1 = sum . map fuel

part_2 :: [Int] -> Int
part_2 = sum . map (sum . takeWhile (> 0) . tail . iterate fuel)

main :: IO ()
main = do
  xs <- map read . words <$> getContents :: IO [Int]
  print $ part_1 xs
  print $ part_2 xs
