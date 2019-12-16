import Data.List.Extra

step :: [Int] -> [Int]
step inp = flip mod 10 . abs .  sum  . step' inp <$> [0..l]
  where
  l = pred $ length inp
  step' is n = zipWith f is (ptn n)
    where
      f a b = a * b

  ptn n = drop 1 $ take (l+2) $ cycle  $ concatMap r base
    where
      base = [0,1,0,-1]
      r = replicate (succ n)

compute :: Int -> [Int] -> String
compute n inp = take 8 $ concatMap show $ (!! n) $ iterate step inp

parse :: String -> [Int]
parse = map read . chunksOf 1

part_1 :: [Int] -> String
part_1 = compute 100

main :: IO ()
main = do
  -- print $ compute 4 $ parse "12345678"
  -- print $ compute 100 $ parse "80871224585914546619083218645595"
  -- print $ compute 100 $ parse "19617804207202209144916044189917"
  -- print $ compute 100 $ parse "69317163492948606335995924319873"

  inp <- parse <$> getLine
  print $ part_1 inp

  -- -- print $ compute 100 inp
  -- --
  -- let t0 = compute 100
  --        $ concat $ replicate 10000 $ parse "03036732577212944063491565474664"
  -- print t0
