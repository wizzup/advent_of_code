readInts :: String -> [Int]
readInts = map read . words

check :: [Int] -> Bool
check [a,b,c] =  a + b > c
              && b + c > a
              && c + a > b

check _       = error "Invalid input"

main :: IO ()
main = do
  ls <- map readInts . lines <$> readFile "input"
  print . length $ filter check ls
