data Button = B1 | B2 | B3 
            | B4 | B5 | B6 
            | B7 | B8 | B9 

instance Show Button where
  show B1 = "1"
  show B2 = "2"
  show B3 = "3"
  show B4 = "4"
  show B5 = "5"
  show B6 = "6"
  show B7 = "7"
  show B8 = "8"
  show B9 = "9"
  
data Move = U | D | L | R
  deriving (Eq, Show, Read)

step :: Move -> Button -> Button
step D B1 = B4
step R B1 = B2

step D B2 = B5
step L B2 = B1
step R B2 = B3

step D B3 = B6
step L B3 = B2

step U B4 = B1
step D B4 = B7
step R B4 = B5

step U B5 = B2
step D B5 = B8
step L B5 = B4  
step R B5 = B6

step U B6 = B3
step D B6 = B9
step L B6 = B5

step U B7 = B4
step R B7 = B8

step U B8 = B5
step L B8 = B7
step R B8 = B9

step U B9 = B6
step L B9 = B8

step _ b = b

step' :: Button -> Move -> Button
step' = flip step

move :: [Move] -> Button -> Button
move xs b = foldl step' b xs

mkMove :: String -> [Move]
mkMove = map (read . (:[]))

main :: IO ()
main = do
  ms <- map mkMove . lines <$> readFile "input" 
  putStrLn . concatMap show . tail $ scanl (flip move) B5 ms
