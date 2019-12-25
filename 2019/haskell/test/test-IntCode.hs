import IntCode

import qualified Data.IntMap as I
import Text.Printf
import Data.Maybe
import Data.List.Extra
import Control.Monad

tests :: Bool
tests = and $ zipWith t inps outs
  where

    t (n, inp, mem) (_, out', mem')
      = dbg $ oval == out' && omem == mem'
      where
        st = runTilHalt def{sMem = loadProg mem, sInputs = inp}
        oval = sOutputs st
        omem = memToProg 0 $ sMem st
        dbg = dbgp $ printf "test #%i" n

    inps :: [(Int,[Int],String)]
    inps = [ (0,  [], "1,0,0,3,99")
           , (1,  [], "1,9,10,3,2,3,11,0,99,30,40,50")
           , (2,  [], "1,0,0,0,99")
           , (3,  [], "2,3,0,3,99")
           , (4,  [], "2,4,4,5,99,0")
           , (5,  [], "1,1,1,4,99,5,6,0,99")
           , (6,  [0],  "3,0,4,0,99")
           , (7,  [1],  "3,0,4,0,99")
           , (8,  [2],  "3,0,4,0,99")
           , (9,  [], "1002,4,3,4,33")
           , (10, [], "1101,100,-1,4,0")
           , (11, [0],  "3,9,8,9,10,9,4,9,99,-1,8")
           , (12, [8],  "3,9,8,9,10,9,4,9,99,-1,8")
           , (13, [7],  "3,9,7,9,10,9,4,9,99,-1,8")
           , (14, [8],  "3,9,7,9,10,9,4,9,99,-1,8")
           , (15, [0],  "3,3,1108,-1,8,3,4,3,99")
           , (16, [8],  "3,3,1108,-1,8,3,4,3,99")
           , (17, [0],  "3,3,1107,-1,8,3,4,3,99")
           , (18, [8],  "3,3,1107,-1,8,3,4,3,99")
           , (19, [0],  "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
           , (20, [2],  "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
           , (21, [0],  "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
           , (22, [2],  "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")

           -- multiple output, no input
           , (23, [],  "104,0,104,1,104,2,99")

           -- multiple inputs, one output
           , (24, [3,7],  "3,0,3,1,1,0,1,2,4,2,99")
           ]

    outs :: [(Int,[Int],String)]
    outs = [ (0, [], "1,0,0,2,99")
           , (1, [], "3500,9,10,70,2,3,11,0,99,30,40,50")
           , (2, [], "2,0,0,0,99")
           , (3, [], "2,3,0,6,99")
           , (4, [], "2,4,4,5,99,9801")
           , (5, [], "30,1,1,4,2,5,6,0,99")
           , (6, [0],  "0,0,4,0,99")
           , (7, [1],  "1,0,4,0,99")
           , (8, [2],  "2,0,4,0,99")
           , (9, [], "1002,4,3,4,99")
           , (10, [], "1101,100,-1,4,99")
           , (11, [0],  "3,9,8,9,10,9,4,9,99,0,8")
           , (12, [1],  "3,9,8,9,10,9,4,9,99,1,8")
           , (13, [1],  "3,9,7,9,10,9,4,9,99,1,8")
           , (14, [0],  "3,9,7,9,10,9,4,9,99,0,8")
           , (15, [0],  "3,3,1108,0,8,3,4,3,99")
           , (16, [1],  "3,3,1108,1,8,3,4,3,99")
           , (17, [1],  "3,3,1107,1,8,3,4,3,99")
           , (18, [0],  "3,3,1107,0,8,3,4,3,99")
           , (19, [0],  "3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9")
           , (20, [1],  "3,12,6,12,15,1,13,14,13,4,13,99,2,1,1,9")
           , (21, [0], "3,3,1105,0,9,1101,0,0,12,4,12,99,0")
           , (22, [1], "3,3,1105,2,9,1101,0,0,12,4,12,99,1")

           -- multiple output, no input
           , (23, [0,1,2],  "104,0,104,1,104,2,99")

           -- multiple inputs, one output
           , (24, [10],  "3,7,10,1,1,0,1,2,4,2,99")
           ]

-- FIXME:: purify/ remove IO
-- Test suites with no mem comparision
outputTests :: Bool
outputTests = and [out0, out1, out2, out3]
  where
    -- day 05 test
    prg0 = concat [ "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
                  , "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
                  , "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                  ]

    check prg inp out = out' == out
      where
        out' = runForOutput prg inp

    out0 = and $ zipWith (check prg0) [[7], [8], [9]] [999, 1000, 1001] 

    -- day 09 tests
    check' prg inp out = out' == out
      where
        out' = runForOutputs prg inp

    -- takes no input and produces a copy of itself as output.
    prg1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    out1 = check' prg1 [] (progToList prg1)

    -- should output a 16-digit number.
    prg2 = "1102,34915192,34915192,7,4,7,99,0"
    out2 = check' prg2 [] [1219070632396864]

    -- should output the large number in the middle.
    prg3 = "104,1125899906842624,99"
    out3 = check' prg3 [] [1125899906842624]

test_02 :: IO Bool
test_02 = do
  putStrLn "Checking day 02 .."
  prog <- readFile "../input/input-02.txt"

  let run i j = sMem st I.! 0
        where
          prog' = memToProg 0
                $ I.adjust (const i) 1
                $ I.adjust (const j) 2
                $ loadProg prog
          st = runTilHalt def{sMem = loadProg prog'}

  let part_1 = 4690667 == run 12 2
  putStrLn $ printf "Part 1 .. %s" (show part_1)

  let pair = [(x,y) | x <- [0..99], y <- [0..99]]
  let st = (\(x,y) -> (run x y, (x,y))) <$> pair
  let (noun, verb) = fromJust $ lookup 19690720 st
  let part_2 = 6255 == 100 * noun + verb
  putStrLn $ printf "Part 2 .. %s" (show part_2)

  pure $ part_1 && part_2

test_05 :: IO Bool
test_05 = do
  putStrLn "Checking day 05 .."
  prog <- readFile "../input/input-05.txt"

  let ost = runForOutput prog [1]
  let part_1 = 15097178 == ost
  putStrLn $ printf "Part 1 .. %s" (show part_1)

  let ost' = runForOutput prog [5]
  let part_2 = 1558663 == ost'
  putStrLn $ printf "Part 2 .. %s" (show part_2)

  pure $ part_1 && part_2

test_07 :: IO Bool
test_07 = do
  putStrLn "Checking day 07 .."
  prog <- readFile "../input/input-07.txt"

  let cfg xs = o4
        where
           [a,b,c,d,e] = xs
           o0 = runForOutput prog [a,0]
           o1 = runForOutput prog [b,o0]
           o2 = runForOutput prog [c,o1]
           o3 = runForOutput prog [d,o2]
           o4 = runForOutput prog [e,o3]

  let out = maximum $ cfg <$> permutations [0,1,2,3,4]
  let part_1 = 929800 == out
  putStrLn $ printf "Part 1 .. %s" (show part_1)

  let config :: Memory -> [Int] -> Int
      config mem [m0,m1,m2,m3,m4] = last ot4
        where
          ot0 = runMem mem (m0:0:ot4)
          ot1 = runMem mem (m1:ot0)
          ot2 = runMem mem (m2:ot1)
          ot3 = runMem mem (m3:ot2)
          ot4 = runMem mem (m4:ot3)
      config _ _ = undefined

  let out' = maximum $ config (loadProg prog) <$> permutations [5..9]
  let part_2 = 15432220 == out'
  putStrLn $ printf "Part 2 .. %s" (show part_2)

  pure $ part_1 && part_2

test_09 :: IO Bool
test_09 = do
  putStrLn "Checking day 09 .."
  prog <- readFile "../input/input-09.txt"

  let part_1 = 2204990589 == runForOutput prog [1]
  putStrLn $ printf "Part 1 .. %s" (show part_1)

  let part_2 = 50008 == runForOutput prog [2]
  putStrLn $ printf "Part 2 .. %s" (show part_2)

  pure $ part_1 && part_2

longTests :: IO ()
longTests = do
  putStrLn "Running longTests .."

  let ts = [(test_02, 02)
           ,(test_05, 05)
           ,(test_07, 07)
           ,(test_09, 09)
           ]

  let ls x = error $ "test " <> show x <> " failed"
  let f (t,n) = t >>= \t' -> unless t' (ls (n::Int))

  sequence_ $ f <$> ts

  putStrLn "Done"

main :: IO ()
main = do
  putStrLn "Running short tests"

  -- quick tests
  unless tests $ error "tests failed"
  unless outputTests $ error "outputTest failed"
  putStrLn "Done"

  -- test with input file (run longer)
  longTests

  putStrLn "done"
