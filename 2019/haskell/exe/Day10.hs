import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.List.Extra
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M


type Pos = (Int,Int)
type PosMap = IntMap Pos -- id and position

mkMap :: [String] -> PosMap
mkMap xs = I.fromList $ zip [0..] ls
  where
    rows = zip [0..] xs
    f (x,y) = zipWith3 g [0..] (repeat x) y
    g x y z = ((x,y),z)
    ls = map fst $ filter h $ concatMap f rows
    -- h (_,z) =  z == '#'
    h (_,z) =  z /= '.'
    -- h  = const True

test0, test1, test2, test3, test4, test5, test6 :: [String]
test0 = [
  "a#cdq",
  "efgh#",
  "ij#ls",
  "#nopt"
  ]

test1 = [
  "#.........",
  "...A......",
  "...B..a...",
  ".EDCG....a",
  "..F.c.b...",
  ".....c....",
  "..efd.c.gb",
  ".......c..",
  "....f...c.",
  "...e..d..c"
  ]

-- #8 (3,4) -> 8
test2 = [
  ".#..#",
  ".....",
  "#####",
  "....#",
  "...##"
  ]

-- (5,8) -> 33
test3 = [
  "......#.#.",
  "#..#.#....",
  "..#######.",
  ".#.#.###..",
  ".#..#.....",
  "..#....#.#",
  "#..#....#.",
  ".##.#..###",
  "##...#..#.",
  ".#....####"
  ]

-- (1,2) -> 35
test4 = [
  "#.#...#.#.",
  ".###....#.",
  ".#....#...",
  "##.#.#.#.#",
  "....#.#.#.",
  ".##..###.#",
  "..#...##..",
  "..##....##",
  "......#...",
  ".####.###."
  ]

-- (6,3) -> 41
test5 = [
  ".#..#..###",
  "####.###.#",
  "....###.#.",
  "..###.##.#",
  "##.##.#.#.",
  "....###..#",
  "..#.#..#.#",
  "#..#.#.###",
  ".##...##.#",
  ".....#.#.."
  ]

-- Best is 11,13 with 210 other asteroids detected:
test6 = [
  ".#..##.###...#######",
  "##.############..##.",
  ".#.######.########.#",
  ".###.#######.####.#.",
  "#####.##.#.##.###.##",
  "..#####..#.#########",
  "####################",
  "#.####....###.#.#.##",
  "##.#################",
  "#####.##.###..####..",
  "..######..##.#######",
  "####.##.####...##..#",
  ".#####..#.######.###",
  "##...#.##########...",
  "#.##########.#######",
  ".####.#.###.###.#.##",
  "....##.##.###..#####",
  ".#.#.###########.###",
  "#.#.#.#####.####.###",
  "###.##.####.##.#..##"
  ]


type DistMap = Map (Pos,Pos) Double

dist :: Pos -> Pos -> Double
dist (a,b) (c,d) = sqrt $ fromIntegral $ x * x + y * y
  where
    x = abs $ a - c
    y = abs $ b - d

dists :: PosMap -> DistMap
dists m = ret
  where
    ms = I.elems m
    ps = [(i,j) | i <- ms, j <- ms, i /= j]
    f (a,b) = ((a,b),dist a b)
    ret = M.fromList $ f <$> ps

-- FIXME: what is complexcity?
--        use polar coord
-- isLineup :: Pos -> Pos -> Pos -> Bool
isLineup dm x y z = or $ check <$> permutations [x,y,z]
  where
    check [a,b,c] = (< 0.0001) $ abs $ dst dm a c - (dst dm a b + dst dm b c)

dst dm x y = M.findWithDefault undefined (x,y) dm

isLineup' dm m i j k = isLineup dm a b c
  where
    [a,b,c] = (\x -> I.findWithDefault undefined x m) <$> [i,j,k]

fnd = I.findWithDefault undefined
fnd' = flip fnd

-- dist' dm m x y = dst dm a b
--   where 
--     a = fnd x m
--     b = fnd y m

-- check if points are not line up
-- snd s->t is longer than s->o (blocked by o)
checkTuple dm m (s,t,o) = check
  where
    [a,b,c] = flip fnd m <$> [s,t,o]
    check | not $ isLineup dm a b c  = True     -- can see each other
          | otherwise = c1
    [(sx,sy),(tx,ty),(ox,oy)] = fnd' m <$> [s,t,o]
    c1 = not (c2 ox sx tx && c2 oy sy ty)    -- x or y is not in s->t range
    c2 a b c = a `elem` [(min b c)..(max b c)]

-- checkPair :: PosMap -> (Int,Int) -> Bool
checkPair dm m (s,t) = ret
  where
   ps = I.keys m
   os = filter (`notElem` [s,t]) ps
   trp = [(s,t,o) | o <- os]
   ret = and $ checkTuple dm m <$> trp
   -- ret = zip trp (checkTuple m <$> trp)

checkPoint dm m i
  | i `I.member` m = sum $ f <$> op
  | otherwise      = error "not member"
  where
    ps = I.keys m
    gp = [(i,j) | j <- ps, i /= j]
    op = checkPair dm m <$> gp
    f t = if t then 1 else 0

checkMap dm m = pp
  where
    ps = I.keys m
    pp = checkPoint dm m <$> ps

checkMap' dm = maximum . checkMap dm . mkMap

tt :: [String] -> Int -> IO ()
tt inp o = do
  print "testing ..."
  let pmap = mkMap inp
  let dm = dists pmap
  -- print $ M.size dm
  -- print $ M.findMin dm
  -- print $ M.findMax dm
  -- mapM_ (print . (\x -> (x,fnd x pmap))) [0..pred $ I.size pmap]
  let res = checkMap dm pmap
  -- print res
  let ans = maximum res
  print ("ans",ans)
  when (ans /= o) $ error $ "test failed, expect: " <> show o
  print "done"

tests :: IO ()
tests = do
  tt test2 8
  tt test4 35
  tt test5 41
  tt test3 33
  tt test6 210

main :: IO ()
main = do
  tests

  pmap <- lines <$> getContents

  -- part 1
  tt pmap 214

  putStrLn "done"

