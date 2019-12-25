import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List.Extra

input :: [[Int]]
input = map toInts $ show <$> [147981..691423]

hasDouble :: [Int] -> Bool
hasDouble xs = or $ zipWith (==) xs (tail xs)

checkDups :: [Int] -> Bool
checkDups xs = chk
  where
    chk = not . null . M.filter (== 2) $ count xs

count :: [Int] -> IntMap Int
count = foldr f M.empty
  where
    f a b | a `M.member` b = M.alter (fmap succ) a b
          | otherwise      = M.insert a 1 b

toInts :: String -> [Int]
toInts = map read <$> chunksOf 1

neverDec :: [Int] -> Bool
neverDec xs = and $ zipWith (<=) xs (tail xs)

tests :: Bool
tests = good_1 && bad_1 && good_2 && bad_2
  where
    good_1 = and $ check_1 . toInts <$> ["111111"]
    bad_1  = and $ not . check_1 . toInts <$> ["123789", "223450"]
    good_2 = and $ check_2 . toInts <$> ["112233", "111122"]
    bad_2  = and $ not . check_2 . toInts <$> ["123444"]

check_1 :: [Int] -> Bool
check_1 xs = (neverDec xs) && (hasDouble xs)

-- 1790
part_1 :: IO ()
part_1 = do
  let ret = length $ filter check_1 input
  print ret

check_2 :: [Int] -> Bool
check_2 xs = (neverDec xs) && (hasDouble xs) && (checkDups xs)

-- 1206
part_2 :: IO ()
part_2 = do
  let ret = length $ filter check_2 input
  print ret

main :: IO ()
main = do
  print tests

  part_1
  part_2

