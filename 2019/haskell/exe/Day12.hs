import Data.Char
import Text.Read (readPrec, lift)
import Text.Printf
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import Control.Monad

data V3Int = V3 Int Int Int
  deriving Eq

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
  deriving Eq

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
  _ <- R.string (c:['='])
  n <- readNumber
  R.skipSpaces
  pure n

readV3Int :: ReadP V3Int
readV3Int = do
  _ <- R.char '<'
  x <- readValue 'x'
  _ <- R.char ','
  y <- readValue 'y'
  _ <- R.char ','
  z <- readValue 'z'
  _ <- R.char '>'
  pure $ V3 x y z


gravity :: Moon -> Moon -> Moon
gravity a b = a'
  where
     a' = a {mVel = va'}
     [dvx,dvy,dvz] = cng <$> zip (toList $ mPos a) (toList $ mPos b)
     [vax',vay',vaz'] = zipWith (+) [dvx,dvy,dvz] (toList $ mVel a)
     va' = V3 vax' vay' vaz'
     cng (x,y)
       | x > y = -1
       | x < y =  1
       | otherwise = 0

simsingle :: Int -> [Moon] -> Moon
simsingle i xs =  a'
  where
    a = xs !! i
    bs = filter ((/= i) . mId) xs
    va = mVel $ foldl gravity a bs
    a' = move a va
    move (Moon j p _) v' = Moon j (p .+ v') v'

step :: [Moon] -> [Moon]
step ms = flip simsingle ms <$> [0..pred $ length ms]

sim :: Int -> [Moon] -> [Moon]
sim n = last . take n . tail . iterate step

toList :: V3Int -> [Int]
toList (V3 i j k) = [i,j,k]

epot :: Moon -> Int
epot m = sum $ abs <$> toList (mPos m)

ekin :: Moon -> Int
ekin m = sum $ abs <$> toList (mVel m)

energy :: Moon -> Int
energy m = epot m * ekin m

inp0 :: [String]
inp0 = [
  "<x=-1, y=0, z=2>",
  "<x=2, y=-10, z=-7>",
  "<x=4, y=-8, z=8>",
  "<x=3, y=5, z=-1>"
  ]

inp1 :: [String]
inp1 = [
  "<x=-8, y=-10, z=0>",
  "<x=5, y=5, z=10>",
  "<x=2, y=-7, z=3>",
  "<x=9, y=-8, z=-3>"
  ]

mkMoons :: [String] -> [Moon]
mkMoons = zipWith f [0..]
  where
    f i p  = Moon i (read p) zero

tests :: Bool
tests = and $ zipWith (==)
  [179
  ,1940]
  [part_1 10  (mkMoons inp0)
  ,part_1 100 (mkMoons inp1)]


part_1 :: Int -> [Moon] -> Int
part_1 n ms = sum $ energy <$> sim n ms

main :: IO ()
main = do
  unless tests $ error "tests failed"

  inp <- lines <$> getContents

  let ms = mkMoons inp
  mapM_ print ms

  -- 8362
  print $ part_1 1000 ms
