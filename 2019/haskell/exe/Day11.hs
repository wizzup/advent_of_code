import qualified Data.Map as M
import Text.Printf
import Debug.Trace

import IntCode

data Color = Black | White
  deriving (Show, Eq)

data Turn = LeftT | RightT
  deriving (Show, Eq)

data Heading = North | East | South | West
  deriving Eq

instance Show Heading where
  show North = "↑"
  show East  = "→"
  show South = "↓"
  show West  = "←"

toTurn :: Int -> Turn
toTurn 0 = LeftT
toTurn 1 = RightT

turnInt :: Turn -> Int
turnInt LeftT  = 0
turnInt RightT = 1

colorInt :: Color -> Int
colorInt Black = 0
colorInt White = 1

toColor :: Int -> Color
toColor 0 = Black
toColor 1 = White

type Pos = (Int,Int)
type PosMap = M.Map Pos Color

next :: Pos -> Heading -> PosMap -> [Int] -> [Int]
next cp h m xs@(x:y:_)
  = trace dbg $ nc: next np nh m' xs'
  where
    (np,nh) = nextPoint cp h (toTurn y)           -- next point/heading
    nc | np `M.member` m = colorInt $ M.findWithDefault undefined np m -- current position color
       | otherwise       = colorInt Black
    xs' = drop 2 xs -- next robot output
    m' = M.alter (const $ Just $ toColor nc) np  -- update this point color
       $ M.alter (const $ Just $ toColor x) cp m -- update last point color
    dbg = printf "p:%s h:%s np:%s sz(%i,%i)\n%s"
            (show cp) (show h) (show np) (M.size m) (M.size m')
            (render m')



config :: Color -> Memory -> [Int]
config c mem = ot0
  where
    ot0 = runMem mem (colorInt c:nxt)
    nxt = next (0,0) North m ot0
    m = M.singleton (0,0) c

nextPoint :: Pos -> Heading -> Turn -> (Pos,Heading)
nextPoint (x,y) North LeftT  = ((x-1,y), West)
nextPoint (x,y) North RightT = ((x+1,y), East)
nextPoint (x,y) East  RightT = ((x,y-1), South)
nextPoint (x,y) East  LeftT  = ((x,y+1), North)
nextPoint (x,y) South RightT = ((x-1,y), West)
nextPoint (x,y) South LeftT  = ((x+1,y), East)
nextPoint (x,y) West  RightT = ((x,y+1), North)
nextPoint (x,y) West  LeftT  = ((x,y-1), South)

robotOutput :: [Int] -> [(Color,Turn)]
robotOutput [] = []
robotOutput (c:t:rest)
  = (c',t') : robotOutput rest
  where
    c' = toColor c
    t' = toTurn t

render :: PosMap -> String
render m = out
  where
    ps = M.keys m
    xs = fst <$> ps
    ys = snd <$> ps
    [mnx,mny] = minimum <$> [xs,ys]
    [mxx,mxy] = maximum <$> [xs,ys]
    out = unlines [[r x y | x <- [mnx..mxx]] | y <- reverse [mny..mxy]]
    c p = M.lookup p m
    r x y | c (x,y) == Just Black = ' '
          | c (x,y) == Just White = '▓'
          | otherwise = '.'

main :: IO ()
main = do
  prog <- getLine
  let mem = loadProg prog

  -- FIXME: extract answer out of trace
  -- part_1: 2160
  let run = config Black mem
  print $ length run

  -- FIXME: extract answer out of trace
  -- part_2: LRZECGFE
  let run2 = config White mem
  print $ length run2

