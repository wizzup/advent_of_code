import Data.List.Split

readInts :: String -> [Int]
readInts = map read . words

check :: [Int] -> Bool
check [a,b,c] =  a + b > c
              && b + c > a
              && c + a > b

check _       = error "Invalid input"

read3s :: [[Int]] -> [[Int]]
read3s [[a,b,c],[d,e,f],[g,h,i]] = [[a,d,g],[b,e,h],[c,f,i]]

read3s _                          = error "Invalid input"

main :: IO ()
main = do
  ls <- concatMap (read3s . map readInts) . chunksOf 3 . lines <$> readFile "input"
  print . length $ filter check ls
