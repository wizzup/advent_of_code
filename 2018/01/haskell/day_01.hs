import qualified Data.Set as S

main :: IO ()
main = interact solve

solve, solveA, solveB :: String -> String
solve s = "A: " ++ solveA s ++ "\nB: " ++ solveB s ++ "\n"

solveA = show . compute . parse
  where compute :: [Int] -> Int
        compute = sum

solveB = show . compute . parse
  where compute :: [Int] -> Maybe Int
        compute = firstRepeated . scanl (+) 0 . cycle

parse :: String -> [Int]
parse = map read' . words

-- |
-- >>> read' "+1"
-- 1
-- >>> read' "-1"
-- -1
--
read' :: String -> Int
read' ('+':n) = read n
read' n       = read n

-- |
-- >>> firstRepeated []
-- Nothing
-- >>> firstRepeated [1,2,3,4,5,1]
-- Just 1
--
firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = go S.empty
  where go :: Ord a => S.Set a -> [a] -> Maybe a
        go _ [] = Nothing
        go s (x:xs)
          | x `S.member` s = Just x
          | otherwise  = go (x `S.insert` s) xs
