---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/9
-- Day7 code re-write with more opcode support
---------------------------------------------------------------------

import Text.ParserCombinators.ReadP (ReadP)
import Control.Monad
import Data.Char
import Data.List.Extra
import Data.Maybe
import qualified Data.IntMap as M
import qualified Text.ParserCombinators.ReadP as R
import Text.Printf

import Debug.Trace

debug :: Bool
-- debug = True
debug = False

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
type Memory = M.IntMap Int

loadProg :: Program -> Memory
loadProg prog = M.fromList . zip [0..] . map read $ splitOn "," prog

memToProg :: Int -> Memory -> String
memToProg n = intercalate "," . map show . drop n . M.elems

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
  = dbgp (printf "runMem pc: %i op: %-25s mem: %s" pc (show op) (show $ dbg mem pc))
  $ case op of
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
    fnd k = M.findWithDefault (error $ "need more ram : " <> show k) k mem
          -- $ M.findWithDefault 0 k mem
    get p = case p of
              Param Position x  -> fnd x
              Param Immediate x -> x
              Param Relative x  -> fnd (rb + x)
    set p = case p of
              Param Position x  -> x
              Param Relative x  -> rb + x
              Param Immediate _ -> error "cant' write to Imm"
    alter v = M.alter (const $ Just v)
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

-- new test program
-- echo itself to output
-- prog0 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

-- out 16 digits number
-- prog1 = "1102,34915192,34915192,7,4,7,99,0"

-- out 1125899906842624
-- prog2 = "104,1125899906842624,99"

main :: IO ()
main = do
  print tests

  prog <- getLine
  let mem = loadProg prog

  -- 2204990589
  let part_1 = print $ last $ fst <$> runMem mem [1]
  part_1

  -- 50008 (very slow)
  let part_2 = print $ last $ fst <$> runMem mem [2]
  part_2
