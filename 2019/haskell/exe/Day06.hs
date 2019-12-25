import Data.Tuple
import Data.Maybe
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as M

test1_map :: OrbitMap
test1_map = mkMap [
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L"]

test2_map :: OrbitMap
test2_map = mkMap [
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L",
  "K)YOU",
  "I)SAN"]

type Pair = (String,String)

toPair :: String -> Pair
toPair xs = (a,b)
  where
    [a,b] = splitOn ")" xs

type OrbitMap = Map String String

mkMap :: [String] -> OrbitMap
mkMap = M.fromList . map (swap . toPair)

get :: String -> OrbitMap -> [String]
get s m = takeWhile (/= "COM") $ iterate look s
  where
    look s' = let fnd = M.lookup s' m in fromMaybe "COM" fnd

-- 278744
part_1 :: OrbitMap -> Int
part_1 m = sum $ length . flip get m <$> ks
  where
    ks = M.keys m

-- 475
part_2 :: OrbitMap -> Int
part_2 m = yn + sn
  where
    you = reverse $ get "YOU" m
    san = reverse $ get "SAN" m
    zz = takeWhile (uncurry (==)) $ zip you san
    lz = length zz
    you' = drop lz you
    san' = drop lz san
    yn = pred $ length you'
    sn = pred $ length san'

main :: IO ()
main = do
  print $ part_1 test1_map
  print $ part_2 test2_map

  mps <- mkMap . lines <$> getContents
  print $ part_1 mps
  print $ part_2 mps
