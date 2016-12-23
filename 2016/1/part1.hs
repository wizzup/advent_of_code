import Data.List.Split
import Control.Monad.State

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
type GameState = (Me, Pos)

startState :: GameState
startState = (startMe, (0,0))
  where startMe = Me { heading = N, pos = (0,0) }

nextState :: [Move] -> State GameState Pos
nextState [] = do
  (_, d) <- get
  return d

nextState (x:xs) = do
  (me, _) <- get
  let newMe = moveStep me x
  put (newMe, pos newMe)
  nextState xs

main :: IO ()
main = do
  moves <- map mkMove . splitOn ", " <$> readFile "input"

  let lastState =  evalState (nextState moves) startState
  print $ (\(x,y) -> abs x + abs y) lastState
