-- {-# LANGUAGE NamedFieldPuns #-}

import Data.Bifunctor
import Data.Char
import Text.Read (readPrec, lift)
import Text.Printf
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import Control.Monad
import Debug.Trace

data V3Int = V3 Int Int Int
  deriving Eq

-- instance Eq V3Int where
--   V3 i j k == V3 x y z = i == x && j == y && k == z

instance Show V3Int where
  show (V3 x y z) = printf "x:%+4i y:%+4i z:%+4i" x y z

instance Read V3Int where
  readPrec = lift readV3Int

(.+) :: V3Int -> V3Int -> V3Int
(.+) (V3 a b c) (V3 d e f)
  = V3 (a+d) (b+e) (c+f)

zero :: V3Int
zero = V3 0 0 0

type Position = V3Int
type Velocity = V3Int

data Moon = Moon { mId :: Int, mPos :: Position, mVel :: Velocity }
  -- deriving Eq

instance Eq Moon where
  Moon i a b == Moon j c d = a == c && b == d

instance Show Moon where
  show (Moon i p v) = printf "%i  p| %s  v| %s" i (show p) (show v)

readNumber :: ReadP Int
readNumber = r0 R.+++ r1
  where
    r0 = read <$> R.munch1 isDigit
    r1 = R.string "-" >> negate <$> r0

readValue :: Char -> ReadP Int
readValue c = do
  R.skipSpaces
  R.string (c:['='])
  n <- readNumber
  R.skipSpaces
  pure n

readV3Int :: ReadP V3Int
readV3Int = do
  R.char '<'
  x <- readValue 'x'
  R.char ','
  y <- readValue 'y'
  R.char ','
  z <- readValue 'z'
  R.char '>'
  pure $ V3 x y z

mkPairs :: [Moon] -> [(Moon,Moon)]
mkPairs ms = [(a,b) | a <- ms, b <- ms, (mId a, mId b) `elem` ps]
  where
    ps = [(0,1), (0,2), (0,3), (1,2), (1,3), (2,3)]

cng :: (Int,Int) -> (Int,Int)
cng (a,b)
  | a > b = (-1,  1)
  | a < b = ( 1, -1)
  | otherwise = (0,0)

g :: Moon -> Moon -> Moon
g a b = a'
  where
     a' = a {mVel = va'}
     b' = b {mVel = vb'}
     [pa,pb] = mPos <$> [a,b]
     (pax,pay,paz) = unpack $ mPos a
     (pbx,pby,pbz) = unpack $ mPos b
     [dvx,dvy,dvz] = cng <$> zip [pax,pay,paz] [pbx,pby,pbz]
     [va,vb] = mVel <$> [a,b]
     (vax,vay,vaz) = unpack $ mVel a
     (vbx,vby,vbz) = unpack $ mVel b
     f (dx,dy) a b = (a+dx,b+dy)
     [(vax',vbx'),(vay',vby'),(vaz',vbz')]
       = zipWith3 f [dvx,dvy,dvz] [vax,vay,vaz] [vbx,vby,vbz]
     va' = V3 vax' vay' vaz'
     vb' = V3 vbx' vay' vbz'

s :: Int -> [Moon] -> Moon
s i xs =  a'
  where
    a = xs !! i
    bs = filter ((/= i) . mId) xs
    va = mVel $ foldl g a bs
    a' = move a va

move :: Moon -> Velocity -> Moon
move (Moon i p v) v' = Moon i (p .+ v') v'

step :: [Moon] -> [Moon]
step ms = flip s ms <$> [0..pred $ length ms]

sim :: Int -> [Moon] -> [Moon]
sim n = last . take n . tail . iterate step

unpack :: V3Int -> (Int,Int,Int)
unpack (V3 i j k) = (i,j,k)

-- epot :: Moon -> Int
epot m = sum $ abs <$> [i,j,k]
  where
    (i,j,k) = unpack $ mPos m

-- ekin :: Moon -> Int
ekin m = sum $ abs <$> [i,j,k]
  where
    (i,j,k) = unpack $ mVel m

energy m = epot m * ekin m

-- eqState :: [Moon] -> [Moon] -> Bool
eqState xs ys = and $ zipWith (==) xs ys

-- FIXME:: always return false
fnd f ms = step' ms 1
  where
    step' is n
      | f os ms = n
      | otherwise = step' os (n+1)
      where
        os = step is

-- findLoop :: [Moon]

inp0 = [
  "<x=-1, y=0, z=2>",
  "<x=2, y=-10, z=-7>",
  "<x=4, y=-8, z=8>",
  "<x=3, y=5, z=-1>"
  ]

inp1 = [
  "<x=-8, y=-10, z=0>",
  "<x=5, y=5, z=10>",
  "<x=2, y=-7, z=3>",
  "<x=9, y=-8, z=-3>"
  ]

-- 8362
part_1 :: [Moon] -> Int
part_1 ms = sum $ energy <$> sim 1000 ms

main :: IO ()
main = do
  inp <- lines <$> getContents
  -- let inp = inp0
  -- mapM_ print inp

  let ms@[a,b,c,d] = (\(i,p) -> Moon i (read p) zero)
                 <$> zip [0..] inp
  mapM_ print ms

  print $ part_1 ms

  -- let f n xs ys = xs !! n == ys !! n
  -- let cycles = flip fnd ms . f <$> [0..3]
  -- print cycles
  -- print $ foldr lcm 1 cycles
