import Data.List.Extra
import qualified Data.Map as M
import qualified Data.IntMap as I
import Debug.Trace
import Text.Printf
import Data.Maybe

import IntCode

part_1 :: Program -> IO ()
part_1 prog = do
  let out = runForOutputs prog []
  let cont = (\[_,_,c] -> if c == 2 then 1 else (0::Int)) <$> chunksOf 3 out
  print $ sum cont

data Object
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  | Score Int
  deriving (Show, Eq)

mkObject :: Int -> Object
mkObject 0 = Empty
mkObject 1 = Wall
mkObject 2 = Block
mkObject 3 = Paddle
mkObject 4 = Ball
mkObject _ = error "unknown object"

render :: PosMap -> String
render m = out
  where
    ps = M.keys m
    xs = fst <$> ps
    ys = snd <$> ps
    [mnx,mny] = minimum <$> [xs,ys]
    [mxx,mxy] = maximum <$> [xs,ys]
    scr = printf "               %s\n"
           (show $ fromJust $ M.lookup (-1,0) m)
    out = unlines [[r x y | x <- [mnx..mxx]] | y <- [mny..mxy]]
       <> scr
    c p = M.lookup p m
    r x y | c (x,y) == Just Empty = ' '
          | c (x,y) == Just Wall  = 'W'
          | c (x,y) == Just Block = 'B'
          | c (x,y) == Just Paddle = '_'
          | c (x,y) == Just Ball = 'O'
          | otherwise = '?'

type Pos = (Int,Int)
type PosMap = M.Map Pos Object

mkPosMap :: [Int] -> PosMap
mkPosMap is = M.fromList ls
  where
    f [-1,0,s] = ((-1,0),Score s)
    f [a,b,c]  = ((a,b),mkObject c)
    f _ = error "invalid object"

    ls = f <$> chunksOf 3 is

find' :: Object -> PosMap -> Pos
find' o m = M.foldrWithKey f (0,0) m
  where
    f k a b = if a == o then k else b

findBall = find' Ball
findPaddle = find' Paddle

play :: Pos -> Pos -> Int
play (b,_) (p,_)
  | b == p = 0
  | b >  p = 1
  | b <  p = -1

next st = st' {sInputs = [cmd]}
  where
    st' = runTilInput st
    out = sOutputs st'
    bp = findBall $ mkPosMap out
    pp = findPaddle $ mkPosMap out
    cmd = play bp pp

run_2 :: String -> IO ()
run_2 prog = do
  let mem'  = I.insert 0 2
            $ loadProg prog
  let st = def{sMem = mem'}
  let st' = runTilInput st
  let out = sOutputs st'
  putStrLn $ render $ mkPosMap out

  let bp = findBall $ mkPosMap out
  let pp = findPaddle $ mkPosMap out
  let cmd = [play bp pp]
  print cmd

  let rnd = render . mkPosMap . sOutputs
  -- putStrLn $ rnd $ next st {sInputs = cmd}
  -- putStrLn $ rnd $ next $ next st {sInputs = cmd}
  -- putStrLn $ rnd $ next $ next $ next st {sInputs = cmd}

  let sts = run st {sInputs = cmd}
  print $ sts
  -- print $ last sts
  -- mapM_ (putStrLn . rnd) $ tail sts
  -- mapM_ (print . sHalt) $ tail sts

run :: State -> State
run inp = out
  where
    out = go inp
    go st | sHalt st = st
          | otherwise = let st' = next st
                        in dbg st' $ go st'
    rnd = render . mkPosMap . sOutputs
    -- rnd = show . M.lookup (-1,0) .  mkPosMap . sOutputs
    dbg st = trace $ rnd st

main :: IO ()
main = do
  prog <- getLine
  putStrLn "loaded"

  -- 301
  part_1 prog

  -- FIXME: almost there
  --        OOM even if collapsed all unfold to manual recursion
  run_2 prog
