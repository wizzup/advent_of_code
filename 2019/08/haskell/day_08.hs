---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/8
---------------------------------------------------------------------

import Data.Char
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as M

type Image = [[Int]]

layers :: Dimension -> [Int] -> [Image]
layers (w,h) xs = chunksOf h $  chunksOf w xs

countZero :: Image -> Int
countZero = countDigit 0

countOne :: Image -> Int
countOne = countDigit 1

countTwo :: Image -> Int
countTwo = countDigit 2

countDigit :: Int -> Image -> Int
countDigit c = length . filter (== c) . concat

-- 2975
part_1 :: [Image] -> Int
part_1 ls = snd . minimum $ f <$> ls
  where
    f xs = (countZero xs, countOne xs * countTwo xs)

mergePicture :: Dimension -> Picture -> Picture -> Picture
mergePicture d a b = out
  where
    out = mkPicture d $ zipWith f xs ys
    [xs,ys] = M.elems <$> [a,b]
    f 0 0 = 0
    f 0 1 = 0
    f 0 2 = 0
    f 1 0 = 1
    f 1 1 = 1
    f 1 2 = 1
    f 2 0 = 0
    f 2 1 = 1
    f 2 2 = 2
    f _ _ = undefined

type Picture = Map (Int,Int) Int
type Dimension = (Int,Int)

mkPicture :: Dimension -> [Int] -> Picture
mkPicture (w,_) xs = M.fromList $ f <$> cells
  where
    rows = zip [0..] $ chunksOf w xs
    cells = concatMap (\(i,rs) -> zip3 (repeat i) [0..] rs) rows
    f (x,y,z) = ((x,y),z)

toImage :: Dimension -> Map (Int,Int) a -> [[a]]
toImage (w,_) p = chunksOf w $ M.elems p

fromImage :: Dimension -> Image -> Picture
fromImage d img = mkPicture d $ concat img

-- EHRUE
part_2 :: Dimension -> [Image] -> IO ()
part_2 dim ls = do
  let pictures = fromImage dim <$> ls
  let combind = foldl1 (mergePicture dim) pictures
  let g 0 = ' '
      g 1 = '▗'
      g _ = ' '
  let combind' = M.map g combind
  -- mapM_ print $ toImage dim combind
  mapM_ putStrLn $ toImage dim combind'

main :: IO ()
main = do
  img <- map digitToInt <$> getLine

  let dim = (25,6)
  let ls =  layers dim img
  print $ part_1 ls
  part_2 dim ls
