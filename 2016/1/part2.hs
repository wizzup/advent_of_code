import Data.List.Split
import Data.Maybe
import Control.Monad.State
import Graphics.Gloss.Geometry.Line

data Dir = N | E | S | W
  deriving Show

data Turn = L | R
  deriving (Show, Read)

data Move = Move { dir :: Turn
                 , step :: Integer 
                 } deriving Show

mkMove :: String -> Move
mkMove [] = undefined
mkMove (x:xs) = Move { dir = read [x], step = read xs }

data Me = Me { heading :: Dir
             , pos     :: (Integer, Integer)
             } deriving Show

moveStep :: Me -> Move -> Me
moveStep m mv = newMe
  where newMe = Me { heading = d, pos = p }
        d = turn (heading m) (dir mv)
        p = move d (pos m) (step mv)
        turn N L = W
        turn N R = E
        turn E L = N
        turn E R = S
        turn S L = E
        turn S R = W
        turn W L = S
        turn W R = N
        move N (x,y) n = (x, y + n)
        move E (x,y) n = (x + n, y)
        move S (x,y) n = (x, y - n)
        move W (x,y) n = (x - n, y)

type Pos = (Integer, Integer)
type GameValue = [Pos]
type GameState = (Me, GameValue)

startState :: GameState
startState = (startMe, [(0,0)])
  where startMe = Me { heading = N, pos = (0,0) }

nextState :: [Move] -> State GameState GameValue
nextState [] = do
  (_, d) <- get
  return d

nextState (x:xs) = do
  (me, ps) <- get
  let newMe = moveStep me x
  put (newMe, pos newMe:ps)
  nextState xs

type Point = (Float, Float)

point :: (Pos, Pos) -> (Point, Point)
point ((a,b),(c,d)) = ((i,j),(k,l))
  where i = fromIntegral a
        j = fromIntegral b
        k = fromIntegral c
        l = fromIntegral d

intersec :: [(Pos, Pos)] -> [Point]
intersec ss = do
  (a,b) <- point <$> ss
  (c,d) <- point <$> ss
  let isp = intersectSegSeg a b c d
  let pt = fromJust isp
  guard $ (a,b) /= (c,d)
  guard $ isJust isp
  guard $ pt `notElem` [a,b,c,d]
  return pt

main :: IO ()
main = do
  moves <- map mkMove . splitOn ", " <$> getLine

  let lastState = reverse $ evalState (nextState moves) startState
  let segments = zip lastState (tail lastState)
  let point = head $ intersec segments
  print $ (\(x,y) -> abs x + abs y) $ (\(x,y) -> (round x, round y)) point
