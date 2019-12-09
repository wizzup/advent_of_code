---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/3
---------------------------------------------------------------------

import Control.Monad (replicateM)
import qualified Data.List.Extra as L
import qualified Data.Set as S

test_in :: [(String,String)]
test_in = [
  ("R8,U5,L5,D3"
  ,"U7,R6,D4,L4"),
  ("R75,D30,R83,U83,L12,D49,R71,U7,L72"
  ,"U62,R66,U55,R34,D71,R55,D58,R83"),
  ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  ,"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  ]

test_out :: [(Int,Int)]
test_out = [(6,30),(159,610),(135,410)]

tests :: Bool
tests = let ret = (\(a,b) -> (part_1 a b, part_2 a b)) <$> test_in
        in ret == test_out

data Dir = U | D | L | R
  deriving Show

data PathPoint = PP Dir Int

instance Show PathPoint where
  show (PP U i) = "↑" ++ show i
  show (PP D i) = "↓" ++ show i
  show (PP L i) = "←" ++ show i
  show (PP R i) = "→" ++ show i

type Path = [PathPoint]

mkPath :: String -> Path
mkPath xs = f <$> L.splitOn "," xs
  where
    f (d:is) = case d of
                 'U' -> PP U (read is)
                 'D' -> PP D (read is)
                 'L' -> PP L (read is)
                 'R' -> PP R (read is)
                 _   -> error "should not be here"
    f _ = error "should not be here"

type Point = (Int,Int)

ppToP :: Point -> PathPoint -> [Point]
ppToP (i,j) (PP U n) = [(i,j + k) | k <- [1..n]]
ppToP (i,j) (PP D n) = [(i,j - k) | k <- [1..n]]
ppToP (i,j) (PP L n) = [(i - k,j) | k <- [1..n]]
ppToP (i,j) (PP R n) = [(i + k,j) | k <- [1..n]]

toPoints :: Path -> [Point]
toPoints = L.foldl f [(0,0)]
  where
    f :: [Point] -> PathPoint -> [Point]
    f ps pp = ps ++ ppToP (last ps) pp

dist :: Point -> Point -> Int
dist (a,b) (c,d) = abs (a-c) + abs (b-d)

part_1 :: String -> String -> Int
part_1 pathA pathB =
  let pa = toPoints $ mkPath pathA
      pb = toPoints $ mkPath pathB
      sa = S.fromList pa
      sb = S.fromList pb
      pts = S.toList $ S.intersection sa sb
  in head . tail . L.sort $ dist (0,0) <$> pts

part_2 :: String -> String -> Int
part_2 pathA pathB =
  let pa = toPoints $ mkPath pathA
      pb = toPoints $ mkPath pathB
      sa = S.fromList pa
      sb = S.fromList pb
      pts = S.toList $ S.filter (/= (0,0)) $  S.intersection sa sb
      la y = (y, length $ takeWhile (/= y) pa)
      xa = L.sort $ la <$> pts
      lb y = (y, length $ takeWhile (/= y) pb)
      xb = L.sort $ lb <$> pts
      out =  zipWith (\(a,b) (_,d) -> (a,b+d)) xa xb
  in snd $ L.minimumOn snd out

main :: IO ()
main = do
  print tests

  [pathA, pathB] <- replicateM 2 getLine
  print $ part_1 pathA pathB
  print $ part_2 pathA pathB
