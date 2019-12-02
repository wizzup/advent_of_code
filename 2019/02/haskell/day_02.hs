---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/2
---------------------------------------------------------------------

import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

type Program = IntMap Int

mkProgram :: [Int] -> Program
mkProgram = M.fromList . zip [0..]

test_inp :: [Program]
test_inp = mkProgram <$> [
  [1,0,0,0,99],
  [2,3,0,3,99],
  [2,4,4,5,99,0],
  [1,1,1,4,99,5,6,0,99]
  ]

test_out :: [Program]
test_out = mkProgram <$> [
  [2,0,0,0,99],
  [2,3,0,6,99],
  [2,4,4,5,99,9801],
  [30,1,1,4,2,5,6,0,99]
  ]

tests :: Bool
tests = and $ zipWith (flip (==) . run ) test_inp test_out

runStep :: Int -> Program -> Program
runStep i p
  | i `M.member` p = case op of
                       1  -> p' (+)
                       2  -> p' (*)
                       _  -> p
  | otherwise      = p
  where
    [op,oa,ob,oc] = look <$> [i..i+3]
    [va,vb] = look <$> [oa,ob]
    look = fromJust . (`M.lookup` p)
    p' o = M.update (const $ Just (va `o` vb)) oc p

run :: Program -> Program
run prg = foldl (flip runStep) prg [0,4..M.size prg+1]

run' :: (Int,Int) -> Program -> Program
run' (x,y) prg
  = let prog = M.update (const $ Just x) 1
             $ M.update (const $ Just y) 2 prg
    in run prog

part_1 :: [Int] -> Int
part_1 = fromJust . M.lookup 0 . run' (12,2) . mkProgram

part_2 :: [Int] -> Int
part_2 xs = 100 * i + j
  where
    is = [0..100]
    prg = mkProgram xs
    prgs = [(0 `M.lookup` run' (x,y) prg, (x, y)) | x <- is, y <- is]
    (i,j) = fromJust $ lookup (Just 19690720) prgs

main :: IO ()
main = do
  print tests

  xs <- map read . splitOn "," <$> getLine :: IO [Int]
  print $ part_1 xs
  print $ part_2 xs
