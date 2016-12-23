data Button =            B1 
                  | B2 | B3 | B4 
             | B5 | B6 | B7 | B8 | B9
                  | BA | BB | BC
                       | BD 

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
  show BA = "A"
  show BB = "B"
  show BC = "C"
  show BD = "D"

data Move = U | D | L | R
  deriving (Show, Read)

step :: Move -> Button -> Button
step D B1 = B3

step D B2 = B6
step R B2 = B3

step U B3 = B1
step D B3 = B7
step L B3 = B2
step R B3 = B4

step D B4 = B8
step L B4 = B3

step R B5 = B6

step U B6 = B2
step D B6 = BA
step L B6 = B5
step R B6 = B7

step U B7 = B3
step D B7 = BB
step L B7 = B6
step R B7 = B8

step U B8 = B4
step D B8 = BC
step L B8 = B7
step R B8 = B9

step L B9 = B8

step U BA = B6
step R BA = BB

step U BB = B7
step D BB = BD
step L BB = BA
step R BB = BC

step U BC = B8
step L BC = BB
step R BC = BC

step U BD = BB

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
