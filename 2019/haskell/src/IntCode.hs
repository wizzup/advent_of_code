module IntCode where

import Text.ParserCombinators.ReadP (ReadP)
import Data.Char
import Data.List.Extra
import Data.Maybe
import qualified Data.IntMap as I
import qualified Text.ParserCombinators.ReadP as R
import Text.Printf

import Debug.Trace
import Control.Monad

debug :: Bool
-- debug = True
debug = False

dbgp :: String -> a -> a
dbgp x y = if debug then trace x y else y

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Show, Eq)

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
    os = map f xs
    f (s,m) = R.string s >> pure (reverse m)
    ms = [(0,p),(1,i),(2,r)] :: [(Int,Mode)]
    xs = filter ((/= "0") . fst)
       $ [g x y z | x <- ms, y <- ms, z <- ms]
    g (x,a) (y,b) (z,c)
      = (show (h x y z), [a,b,c])
    h a b c = a * 100 + b * 10 + c

type Program = String
type Memory = I.IntMap Int

loadProg :: Program -> Memory
loadProg prog = I.fromList . zip [0..] . map read $ splitOn "," prog

progToList :: Program -> [Int]
progToList = I.elems . loadProg

memToProg :: Int -> Memory -> String
memToProg n = intercalate "," . map show . drop n . I.elems

peekOp :: Memory -> Int -> Maybe Operation
peekOp mem pc
  = fst <$> listToMaybe ( R.readP_to_S readOper $ memToProg pc mem)

type Input = Int
type Output = Int
type PC = Int
type Base = Int

data State = State
  { sMem :: Memory
  , sPC :: PC
  , sBase :: Base
  , sHalt :: Bool
  , sWait :: Bool
  , sInputs :: [Input]
  , sOutputs :: [Output]
  }

def :: State
def = State
  { sMem     = I.empty
  , sPC      = 0
  , sBase    = 0
  , sHalt    = False
  , sWait    = False
  , sInputs  = []
  , sOutputs = []
  }

instance Show State where
  show (State mem pc base h w ins ots)
    = printf
        "{in:%s out:%s h:%s w:%s pc:%i rb:%i mem:%s}"
        -- "{in:%s out:%s h:%s w:%s pc:%i rb:%i}"
        (slist ins) (slist ots) (sbool h) (sbool w) pc base
        (memToProg 0 mem)
    where
      slist []    = "[]"
      slist (x:_) = "(" <> show x <> ":..)"
      sbool True  = "T"
      sbool False = "F"

-- run program one instruction
runStep :: State -> State
runStep si
  = dbg $ case op of
            Halt      -> State mem pc rb True False ins ots
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
    -- NOTE: cant show input because it might not completely build
    --       and will create <<loop>>
    dbg = dbgp (printf "runStep: <%s" (show si))
    (mem,pc,rb,ins,ots) = (sMem si, sPC si, sBase si, sInputs si, sOutputs si)
    op = case peekOp mem pc of
          Just x  -> x
          Nothing -> error $ printf "invalid op: pc:%i" pc
    fnd k = I.findWithDefault 0 k mem
    get p = case p of
              Param Position x  -> fnd x
              Param Immediate x -> x
              Param Relative x  -> fnd (rb + x)
    set p = case p of
              Param Position x  -> x
              Param Relative x  -> rb + x
              Param Immediate _ -> error "cant' write to Imm"
    -- alter v = I.alter (const $ Just v)
    alter v k m = I.insert k v m
    doOutput x = State mem  (pc + 2) rb False False ins  ots'
      where
        -- FIXME: append vs prepend
        ots' = ots ++ [get x]
    doInput  x = State mem' (pc + 2) rb False w'    ins' ots
      where
        w' = null ins
        (ins', mem') = case uncons ins of
                         Just (i,is) -> (is, alter i (set x) mem)
                         Nothing     -> error "runStep: need input"
    doRAdj   x = State mem  (pc + 2) rb' False False ins ots
      where
        rb' = rb + get x
    doJnz x y = State mem tgt rb False False ins ots
      where
        tgt = if get x /= 0 then get y else pc + 3
    doJz  x y = State mem tgt rb False False ins ots
      where
        tgt = if get x == 0 then get y else pc + 3
    doAdd x y z = State mem' (pc + 4) rb False False ins ots
      where
        mem' = alter (get x + get y) (set z) mem
    doMul x y z = State mem' (pc + 4) rb False False ins ots
      where
        mem' = alter (get x * get y) (set z) mem
    doEq  x y z = State mem' (pc + 4) rb False False ins ots
      where
        mem' = alter (if get x == get y then 1 else 0) (set z) mem
    doLt  x y z = State mem' (pc + 4) rb False False ins ots
      where
        mem' = alter (if get x < get y then 1 else 0) (set z) mem

-- FIXME: why this version failed day_13 part_2?
runTil :: (State -> Bool) -> State -> State
runTil f inp = out
  where
    out = go inp
    go st | f (runStep st) = st
          | otherwise = go $ runStep st

-- runTil :: (State -> Bool) -> State -> State
-- runTil f inp = last out
--   where
--     out = go inp
--     go st | f st = []
--           | otherwise = let st' = runStep st
--                         in st:go st'

runTilOut :: State -> State
runTilOut st = dbg out'
  where
    out = runTil (\s -> sHalt s || (not . null $ sOutputs s)) st
    out' = runStep out
    dbg = dbgp $ printf "runTilOut: >%s" (show out)

runTilInput :: State -> State
runTilInput st = dbg out
  where
    out = runTil sWait st
    dbg = dbgp $ printf "runTilInput: >%s" (show out)
    -- out' = runStep out

runTilHalt :: State -> State
runTilHalt st = dbg out
  where
    -- sinit = def { sMem = loadProg prg, sInputs = inp }
    -- f st' | sHalt st' = Nothing
    --       | otherwise = Just (st', runStep st')
    -- out' = unfoldr f sinit
    out = runStep out'
    out' = runTil sHalt st
    dbg = dbgp $ printf "runTilHalt: >%s" (show out)


-- FIXME: can't break the <<loop>> using runStep
-- FIXME: this fail day_07 test
runMem :: Memory -> [Input] -> [Output]

-- runMem mem ins = go $ def {sMem = mem, sInputs = ins}
--   where
--     go st | sHalt st = []
--           | null (sOutputs st) = go st'
--           | otherwise = head (sOutputs st) : go st'
--       where
--         st' = runStep st
runMem = runMem'

-- run step with lazy input
runMem' :: Memory -> [Input] -> [Output]
runMem' mm inp = go mm inp 0 0
  where
    go mem ins pc rb
      = dbg $ case op of
      -- = case op of
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
        dbg = dbgp
            $ printf "runMem in:%s pc:%i rb:%i op:%s mem:%s"
              (if null inp then "[]" else "(" <> show (head inp) <> ":?)")
              pc rb (show op) (memToProg 0 mem)
        op = case peekOp mem pc of
              Just x  -> x
              Nothing -> error "invalid operation"
        fnd k = I.findWithDefault 0 k mem
        get p = case p of
                  Param Position x  -> fnd x
                  Param Immediate x -> x
                  Param Relative x  -> fnd (rb + x)
        set p = case p of
                  Param Position x  -> x
                  Param Relative x  -> rb + x
                  Param Immediate _ -> error "cant' write to Imm"
        alter v = I.alter (const $ Just v)
        doInput  x = go mem' ins' (pc + 2) rb
          where
            ins' = tail ins
            mem' = case ins of
                    (i:_) -> alter i (set x) mem
                    []    -> error "runStep: need input"
        doOutput x = get x : go mem ins (pc + 2) rb
        doRAdj   x = go mem ins (pc + 2) rb'
          where
            rb' = rb + get x
        doJnz x y = go mem ins tgt rb
          where
            tgt = if get x /= 0 then get y else pc + 3
        doJz  x y = go mem ins tgt rb
          where
            tgt = if get x == 0 then get y else pc + 3
        doAdd x y z = go mem' ins (pc + 4) rb
          where
            mem' = alter (get x + get y) (set z) mem
        doMul x y z = go mem' ins (pc + 4) rb
          where
            mem' = alter (get x * get y) (set z) mem
        doEq  x y z = go mem' ins (pc + 4) rb
          where
            mem' = alter (if get x == get y then 1 else 0) (set z) mem
        doLt  x y z = go mem' ins (pc + 4) rb
          where
            mem' = alter (if get x < get y then 1 else 0) (set z) mem

runForOutput :: String -> [Input] -> Output
runForOutput = (last .) . runForOutputs

runForOutputs :: String -> [Input] -> [Output]
runForOutputs prg inp
  = sOutputs $ runTilHalt $ def{sMem = loadProg prg, sInputs =  inp}
