import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List.Extra
import Control.Monad

type Pos = (Int,Int)
type PosMap = IntMap Pos -- id and position

mkMap :: [String] -> PosMap
mkMap xs = M.fromList $ zip [0..] ls
  where
    rows = zip [0..] xs
    f (x,y) = zipWith3 g [0..] (repeat x) y
    g x y z = ((x,y),z)
    ls = map fst $ filter h $ concatMap f rows
    -- h (_,z) =  z == '#'
    h (_,z) =  z /= '.'
    -- h  = const True

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

dist :: Pos -> Pos -> Double
dist (a,b) (c,d) = sqrt $ fromIntegral $ x * x + y * y
  where
    x = abs $ a - c
    y = abs $ b - d

-- FIXME: what is complexcity?
--        use polar coord
isLineup :: Pos -> Pos -> Pos -> Bool
isLineup x y z = or $ check <$> permutations [x,y,z]
  where
    check [a,b,c] = (< 0.0001) $ abs $ dist a c - (dist a b + dist b c)

isLineup' m i j k = isLineup a b c
  where
    [a,b,c] = (\x -> M.findWithDefault undefined x m) <$> [i,j,k]

fnd = M.findWithDefault undefined
fnd' = flip fnd

dist' m x y =  dist a b
  where 
    a = fnd x m
    b = fnd y m

-- check if points are not line up
-- snd s->t is longer than s->o (blocked by o)
checkTuple m (s,t,o) = check
  where
    [a,b,c] = flip fnd m <$> [s,t,o]
    check | not $ isLineup a b c  = True     -- can see each other
          | otherwise = c1
    [(sx,sy),(tx,ty),(ox,oy)] = fnd' m <$> [s,t,o]
    c1 = not (c2 ox sx tx && c2 oy sy ty)    -- x or y is not in s->t range
    c2 a b c = a `elem` [(min b c)..(max b c)]

-- checkPair :: PosMap -> (Int,Int) -> Bool
checkPair m (s,t) = ret
  where
   ps = M.keys m
   os = filter (`notElem` [s,t]) ps
   trp = [(s,t,o) | o <- os]
   ret = and $ checkTuple m <$> trp
   -- ret = zip trp (checkTuple m <$> trp)

checkPoint m i
  | i `M.member` m = sum $ f <$> op
  | otherwise      = error "not member"
  where
    ps = M.keys m
    gp = [(i,j) | j <- ps, i /= j]
    op = checkPair m <$> gp
    f t = if t then 1 else 0

checkMap m = pp
  where
    ps = M.keys m
    pp = checkPoint m <$> ps

checkMap' = maximum . checkMap . mkMap

tt inp o = do
  let pmap = mkMap inp
  mapM_ (print . (\x -> (x,fnd x pmap))) [0..pred $ M.size pmap]
  let res = checkMap pmap
  print res
  let ans = maximum res
  print ans
  when (ans /= o) $ error $ "test failed, expect: " <> show o

tests = do
  tt test2 8
  tt test4 35
  tt test5 41
  tt test3 33
  tt test6 210

main :: IO ()
main = do
  -- tests

  pmap <- lines <$> getContents

  -- part 1
  tt pmap 214

  putStrLn "done"

