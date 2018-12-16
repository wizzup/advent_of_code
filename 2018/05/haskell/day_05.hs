import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = "A: " ++ fst res ++ "\nB: " ++ snd res ++ "\n"
  where res = (solveA cs, solveB cs)
        -- NOTE: of-by-one error, input contain endline char
        cs = filter isAlpha xs

-- |
-- >>> isReact 'a' 'a'
-- False
-- >>> isReact 'a' 'A'
-- True
-- >>> isReact 'a' 'B'
-- False
isReact :: Char -> Char -> Bool
isReact a b = a /= b && toUpper a == toUpper b

-- | NOTE: To my surprise, not really sure why it is NOT The case that this fold
--         will only do one-pass
-- assume tails is already reduce any con of a new head will also reduce?
--
-- dabAcCaCBAcCcaDA
-- dabA11aCBAc11aDA
-- dab2112CBAc11aDA
-- dabCBAcaDA
--
-- >>> reduce "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
--
reduce :: String -> String
reduce = foldr f []
  where f :: Char -> String -> String
        f c (x:xs) | isReact c x = xs
                   | otherwise   = c:x:xs
        f c [] = [c]

solveA :: String -> String
solveA = show . length . reduce

-- |
-- >>> removeUnit 'a' "dabAcCaCBAcCcaDA"
-- "dbcCCBcCcD"
-- >>> removeUnit 'A' "dabAcCaCBAcCcaDA"
-- "dbcCCBcCcD"
-- >>> removeUnit 'b' "dabAcCaCBAcCcaDA"
-- "daAcCaCAcCcaDA"
-- >>> removeUnit 'c' "dabAcCaCBAcCcaDA"
-- "dabAaBAaDA"
-- >>> removeUnit 'd' "dabAcCaCBAcCcaDA"
-- "abAcCaCBAcCcaA"
--
removeUnit :: Char -> String -> String
removeUnit c = filter (`notElem` [toUpper c, toLower c])

solveB :: String -> String
solveB xs = show r 
  where r = minimum $ map (length . reduce . flip removeUnit xs) ['a'..'z']
