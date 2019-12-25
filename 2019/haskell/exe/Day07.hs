import Data.List

import IntCode

cfg :: String -> [Input] -> Output
cfg prog xs = o4
  where
      [a,b,c,d,e] = xs
      o0 = runForOutput prog [a,0]
      o1 = runForOutput prog [b,o0]
      o2 = runForOutput prog [c,o1]
      o3 = runForOutput prog [d,o2]
      o4 = runForOutput prog [e,o3]

config :: Memory -> [Int] -> Int
config mem [m0,m1,m2,m3,m4] = last ot4
  where
    ot0 = runMem mem (m0:0:ot4)
    ot1 = runMem mem (m1:ot0)
    ot2 = runMem mem (m2:ot1)
    ot3 = runMem mem (m3:ot2)
    ot4 = runMem mem (m4:ot3)
config _ _ = undefined

-- 929800
part_1 :: String -> Output
part_1 prog = maximum $ (cfg prog) <$> permutations [0,1,2,3,4]

-- 15432220
part_2 :: String -> Output
part_2 prog = maximum $ config (loadProg prog) <$> permutations [5..9]

main :: IO ()
main = do
  prog <- getLine
  print $ part_1 prog
  print $ part_2 prog
