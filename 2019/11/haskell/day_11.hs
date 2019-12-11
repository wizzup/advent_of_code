---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/11
-- Day9 code re-write for paint bot
---------------------------------------------------------------------

import Text.ParserCombinators.ReadP (ReadP)
import Data.Char
import Data.List.Extra
import Data.Maybe
import qualified Data.IntMap as I
import qualified Text.ParserCombinators.ReadP as R
import Text.Printf
import Text.Read (readPrec, lift)

import Debug.Trace
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

debug :: Bool
debug = True
-- debug = False

dbgp :: String -> a -> a
dbgp x y = if debug then trace x y else y

data Mode
  = Position
  | Immediate
  | Relative
  deriving Show

data Param = Param Mode Int

instance Show Param where
  show (Param Position i)  = "@" <> show i
  show (Param Immediate i) = show i
  show (Param Relative i)  = if i >= 0
                             then "R+" <> show i
                             else "R-" <> show (negate i)

data Operation
  = Add Param Param Param
  | Mul Param Param Param
  | Eq  Param Param Param
  | Lt  Param Param Param
  | Jnz Param Param
  | Jz  Param Param
  | Input  Param
  | Output Param
  | RAdj   Param
  | Halt
  deriving Show

readOper :: ReadP Operation
readOper = foldr (R.+++) R.pfail [
             readAdd,
             readMul,
             readJz,
             readJnz,
             readLt,
             readEq,
             readRAdj,
             readOut,
             readInp,
             readHalt
           ]

readInt :: ReadP Int
readInt = (R.char '-' >> (negate . read <$> R.munch1 isDigit))
    R.+++ (read <$> R.munch1 isDigit)

readComma :: ReadP ()
readComma = R.char ',' >> pure ()

read1 :: String -> (Param -> Operation) -> ReadP Operation
read1 s f = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string s
      readComma
      f . Param Position <$> readInt
    r1 = do
      (m:_) <- readMode
      _ <- R.string ("0"<>s)
      readComma
      f . Param m <$> readInt

read2 :: String -> (Param -> Param -> Operation) -> ReadP Operation
read2 c f = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string c
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ f (Param Position a) (Param Position b)
    r1 = do
      (i:j:_) <- readMode
      _ <- R.string ("0"<>c)
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ f (Param i a) (Param j b)

read3 :: String -> (Param -> Param -> Param -> Operation) -> ReadP Operation
read3 s f = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string s
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ f (Param Position a) (Param Position b) (Param Position c)
    r1 = do
      (i:j:k:_) <- readMode
      _ <- R.string ("0"<>s)
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ f (Param i a) (Param j b) (Param k c)

readAdd :: ReadP Operation
readAdd = read3 "1" Add

readMul :: ReadP Operation
readMul = read3 "2" Mul

readInp :: ReadP Operation
readInp = read1 "3" Input

readOut :: ReadP Operation
readOut = read1 "4" Output

readJnz :: ReadP Operation
readJnz = read2 "5" Jnz

readJz :: ReadP Operation
readJz = read2 "6" Jz

readLt :: ReadP Operation
readLt = read3 "7" Lt

readEq :: ReadP Operation
readEq = read3 "8" Eq

readRAdj :: ReadP Operation
readRAdj = read1 "9" RAdj

readHalt :: ReadP Operation
readHalt = R.string "99" >> pure Halt

readMode :: ReadP [Mode]
readMode = foldr (R.+++) R.pfail os
  where
    (p,i,r) = (Position,Immediate,Relative)
    os = map f ls
    f (s,m) = R.string s >> pure (reverse m)
    ls = [(  "1",[p,p,i]),
          (  "2",[p,p,r]),
          ( "10",[p,i,p]),
          ( "11",[p,i,i]),
          ( "12",[p,i,r]),
          ( "20",[p,r,p]),
          ( "21",[p,r,i]),
          ( "22",[p,r,r]),
          ("100",[i,p,p]),
          ("101",[i,p,i]),
          ("102",[i,p,r]),
          ("110",[i,i,p]),
          ("111",[i,i,i]),
          ("112",[i,i,r]),
          ("120",[i,r,p]),
          ("121",[i,r,i]),
          ("122",[i,r,r]),
          ("200",[r,p,p]),
          ("201",[r,p,i]),
          ("202",[r,p,r]),
          ("210",[r,i,p]),
          ("211",[r,i,i]),
          ("212",[r,i,r]),
          ("220",[r,r,p]),
          ("221",[r,r,i]),
          ("222",[r,r,r])]

test0,test1,test2,test3,test4,test5 :: (Program, Int, Int)
test0 = ("3,9,8,9,10,9,4,9,99,-1,8", 8, 1)  -- inp == 8 -> out = 1
test1 = ("3,9,7,9,10,9,4,9,99,-1,8", 7, 1)  -- inp < 8  -> out = 1
test2 = ("3,3,1108,-1,8,3,4,3,99",   8, 1)  -- inp == 8 -> out = 1
test3 = ("3,3,1107,-1,8,3,4,3,99",   7, 1)   -- inp < 8  -> out = 1
test4 = ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 0, 0)  -- inp == 0 -> out = 0 else 1
test5 = ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 5, 1)  -- inp == 0 -> out = 0 else 1

tests :: Bool
tests = and $ test <$> [test0,test1,test2,test3,test4,test5]
  where
    test (prog, inp, out) = (== out) . fst . last $ runMem (loadProg prog) [inp]

type Program = String
type Memory = I.IntMap Int

loadProg :: Program -> Memory
loadProg prog = I.fromList . zip [0..] . map read $ splitOn "," prog

memToProg :: Int -> Memory -> String
memToProg n = intercalate "," . map show . drop n . I.elems

type Input = [Int]
type PC = Int
type Output = Int
type RelativeBase = Int

peekOp :: Memory -> Int -> Maybe Operation
peekOp mem pc
  = fst <$> listToMaybe ( R.readP_to_S readOper $ memToProg pc mem)

runMem :: Memory -> Input -> [(Output,Program)]
runMem mem ins = runMem' mem ins 0 0

runMem' :: Memory -> Input -> PC -> RelativeBase -> [(Output,Program)]
runMem' mem ins pc rb
  -- = dbgp (printf "runMem pc: %i op: %-25s mem: %s" pc (show op) (show $ dbg mem pc))
  = case op of
        Halt      -> []
        Input  x  -> doInput  x
        Output x  -> doOutput x
        RAdj   x  -> doRAdj   x
        Jnz x y   -> doJnz x y
        Jz  x y   -> doJz  x y
        Add x y z -> doAdd x y z
        Mul x y z -> doMul x y z
        Lt  x y z -> doLt x y z
        Eq  x y z -> doEq x y z
  where
    dbg mm p = "(" <> show p <> ") " <> take 20 (memToProg p mm)
    op = case peekOp mem pc of
          Just x  -> x
          Nothing -> error $ "invalid operation: " <> dbg mem pc
    fnd k = I.findWithDefault (error $ "need more ram : " <> show k) k mem
          -- $ I.findWithDefault 0 k mem
    get p = case p of
              Param Position x  -> fnd x
              Param Immediate x -> x
              Param Relative x  -> fnd (rb + x)
    set p = case p of
              Param Position x  -> x
              Param Relative x  -> rb + x
              Param Immediate _ -> error "cant' write to Imm"
    alter v = I.alter (const $ Just v)
    doOutput x = (get x, memToProg pc mem) : runMem' mem ins (pc + 2) rb
    doRAdj x = runMem' mem ins (pc + 2) (rb + get x)
    doJnz x y = runMem' mem ins tgt rb
      where
        tgt = if get x /= 0 then get y else pc + 3
    doJz x y = runMem' mem ins tgt rb
      where
        tgt = if get x == 0 then get y else pc + 3
    doInput x = runMem' mem' (tail ins) (pc + 2) rb
      where
        mem' = alter (head ins) (set x) mem
    doAdd x y z = runMem' mem' ins (pc + 4) rb
      where
        mem' = alter (get x + get y) (set z) mem
    doMul x y z = runMem' mem' ins (pc + 4) rb
      where
        mem' = alter (get x * get y) (set z) mem
    doEq x y z = runMem' mem' ins (pc + 4) rb
      where
        mem' = alter (if get x == get y then 1 else 0) (set z) mem
    doLt x y z = runMem' mem' ins (pc + 4) rb
      where
        mem' = alter (if get x < get y then 1 else 0) (set z) mem

config :: Color -> Memory -> [Int]
config c mem = ot0
  where
    ot0 = fst <$> runMem mem (0:nxt)
    nxt = next (0,0) North M.empty ot0
    m = M.singleton (0,0) c

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
type PosMap = Map Pos Color

next :: Pos -> Heading -> PosMap -> [Int] -> [Int]
next cp h m xs@(x:y:_)
  = dbgp dbg $ nc: next np nh m' xs'
  where
    (np,nh) = nextPoint cp h (toTurn y)           -- next point/heading
    nc | np `M.member` m = colorInt $ M.findWithDefault undefined np m -- current position color
       | otherwise       = colorInt Black
    xs' = drop 2 xs -- next robot output
    m' = M.alter (const $ Just $ toColor nc) np -- update this point color
       $ M.alter (const $ Just $ toColor x) cp m   -- update last point color
    dbg = printf "vvv\n%s %s : %s %s\n%s\n^^^"
          (show cp) (show h) (show $ toColor x) (show $ toTurn y)
          (show $ M.size m)

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

main :: IO ()
main = do
  unless tests $ error "test failed"

  prog <- getLine
  let mem = loadProg prog

  -- part_1
  -- 2160 : (2159 + 1) answer is in trace message
  -- FIXME: extract answer
  -- let run = config Black mem
  -- print run

  -- TODO: painting
  -- part_2
  let run = config White mem
  print run
