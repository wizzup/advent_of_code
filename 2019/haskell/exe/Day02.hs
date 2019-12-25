import Data.IntMap ((!), adjust)
import Data.Maybe

import IntCode

run :: String -> Int -> Int -> Int
run prog i j = sMem st ! 0
  where
    prog' = memToProg 0
          $ adjust (const i) 1
          $ adjust (const j) 2
          $ loadProg prog
    st = runTilHalt def{sMem = loadProg prog'}

part_1 :: String -> Int
part_1 prog = run prog 12 2

part_2 :: String -> Int
part_2 prog = 100 * noun + verb
  where
    pair = [(x,y) | x <- [0..99], y <- [0..99]]
    st = (\(x,y) -> (run prog x y, (x,y))) <$> pair
    (noun, verb) = fromJust $ lookup 19690720 st

main :: IO ()
main = do
  prog <- getLine

  putStrLn "Part 1 .."
  print $ part_1 prog

  putStrLn "Part 2 .."
  print $ part_2 prog
