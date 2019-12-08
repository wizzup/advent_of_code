---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/7
-- Day5 code re-write with multiple input output
---------------------------------------------------------------------
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec)
import Control.Monad
import Data.Char
import Data.List.Extra
import Data.Maybe
import qualified Data.IntMap as M
import qualified Text.ParserCombinators.ReadP as R

data Operation
  = Add Mode Int Int Int
  | Mul Mode Int Int Int
  | Eq Mode Int Int Int
  | Lt Mode Int Int Int
  | Jnz Mode Int Int
  | Jz Mode Int Int
  | Input Mode Int
  | Output Mode Int
  | Halt
  deriving Show

instance Read Operation where
  readPrec = lift readOper
  readList = R.readP_to_S readCode

readOper :: ReadP Operation
readOper = foldr (R.+++) R.pfail [
             readHalt,
             readOut,
             readInp,
             readMul,
             readAdd,
             readJz,
             readJnz,
             readLt,
             readEq
           ]

readHalt :: ReadP Operation
readHalt = R.string "99" >> pure Halt

readInt :: ReadP Int
readInt = (R.char '-' >> (negate . read <$> R.munch1 isDigit))
    R.+++ (read <$> R.munch1 isDigit)

readComma :: ReadP ()
readComma = R.char ',' >> pure ()

readOut :: ReadP Operation
readOut = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "4"
      readComma
      Output M000 <$> readInt
    r1 = do
      mode <- readMode
      _ <- R.string "04"
      readComma
      Output mode <$> readInt

readInp :: ReadP Operation
readInp = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "3"
      readComma
      Input M000 <$> readInt
    r1 = do
      mode <- readMode
      _ <- R.string "03"
      readComma
      Input mode <$> readInt

readMul :: ReadP Operation
readMul = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "2"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Mul M000 a b c
    r1 = do
      mode <- readMode
      _ <- R.string "02"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Mul mode a b c

readAdd :: ReadP Operation
readAdd = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "1"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Add M000 a b c
    r1 = do
      mode <- readMode
      _ <- R.string "01"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Add mode a b c

readEq :: ReadP Operation
readEq = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "8"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Eq M000 a b c
    r1 = do
      mode <- readMode
      _ <- R.string "08"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Eq mode a b c

readLt :: ReadP Operation
readLt = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "7"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Lt M000 a b c
    r1 = do
      mode <- readMode
      _ <- R.string "07"
      [a,b,c] <- replicateM 3 (readComma >> readInt)
      pure $ Lt mode a b c

readJnz :: ReadP Operation
readJnz = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "5"
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ Jnz M000 a b
    r1 = do
      mode <- readMode
      _ <- R.string "05"
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ Jnz mode a b

readJz :: ReadP Operation
readJz = r0 R.+++ r1
  where
    r0 = do
      _ <- R.string "6"
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ Jz M000 a b
    r1 = do
      mode <- readMode
      _ <- R.string "06"
      [a,b] <- replicateM 2 (readComma >> readInt)
      pure $ Jz mode a b

data Mode
  = M000
  | M001
  | M010
  | M011
  | M100
  | M101
  | M110
  | M111
  deriving Show

readMode :: ReadP Mode
readMode = (R.string "1"   >> pure M001)
     R.+++ (R.string "10"  >> pure M010)
     R.+++ (R.string "11"  >> pure M011)
     R.+++ (R.string "100" >> pure M100)
     R.+++ (R.string "101" >> pure M101)
     R.+++ (R.string "110" >> pure M110)
     R.+++ (R.string "111" >> pure M111)

readStep :: ReadP Operation
readStep = (readOper <* readComma) R.<++ readOper

type Code = [Operation]

readCode :: ReadP Code
readCode = R.many (readStep R.+++ readOper)

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
    test (prog, inp, out) = (== out) . last $ runMem (loadProg prog) [inp] 0

type Program = String
type Memory = M.IntMap Int

loadProg :: Program -> Memory
loadProg = M.fromList . zip [0..] . map read . splitOn ","

memToProg' :: Int -> Memory -> String
memToProg' n = intercalate "," . map show . drop n . M.elems

type Input = [Int]
type PC = Int

peekOp :: Memory -> Int -> Maybe Operation
peekOp mem pc = fst <$> listToMaybe ( R.readP_to_S readOper $ memToProg' pc mem)

runMem :: Memory -> Input -> PC -> [Int]
runMem mem ins pc
  = case op of
        Halt        -> []
        Input m x   -> doInput m x
        Add m x y z -> doAdd m x y z
        Jnz m x y   -> doJnz m x y
        Mul m x y z -> doMul m x y z
        Output m x  -> doOutput m x
        Jz m x y    -> doJz m x y
        Lt m x y z  -> doLt m x y z
        Eq m x y z  -> doEq m x y z
  where
    op = case peekOp mem pc of
          Just x  -> x
          Nothing -> error "invalid operation"
    fnd k = M.findWithDefault 0 k mem
    doInput m x = runMem mem' (tail ins) (pc + 2)
      where
        mem' = case m of
                 M000 -> M.adjust (const (head ins)) x mem
                 _    -> error "cant write im val"
    doAdd m x y z = runMem mem' ins (pc + 4)
      where
        mem' = case m of
                M000 -> M.adjust (const $ fnd x + fnd y) z mem
                M001 -> M.adjust (const $     x + fnd y) z mem
                M010 -> M.adjust (const $ fnd x +     y) z mem
                M011 -> M.adjust (const $     x +     y) z mem
                _    -> error "cant write im val"
    doJnz m x y = runMem mem ins tgt
      where
        tgt = if x' /= 0 then y' else pc + 3
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "too many params"
    doJz m x y = runMem mem ins tgt
      where
        tgt = if x' == 0 then y' else pc + 3
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "too many params"
    doMul m x y z = runMem mem' ins (pc + 4)
      where
        mem' = case m of
                M000 -> M.adjust (const $ fnd x * fnd y) z mem
                M001 -> M.adjust (const $     x * fnd y) z mem
                M010 -> M.adjust (const $ fnd x *     y) z mem
                M011 -> M.adjust (const $     x *     y) z mem
                _    -> error "cant write im val"
    doOutput m x = opt : runMem mem ins (pc + 2)
      where
        opt = case m of
                M000 -> fnd x
                M001 -> x
                _    -> error "too many params"
    doEq m x y z = runMem mem' ins (pc + 4)
      where
        mem' = M.adjust (const $ if x' == y' then 1 else 0) z mem
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "cant write im val"
    doLt m x y z = runMem mem' ins (pc + 4)
      where
        mem' = M.adjust (const $ if x' < y' then 1 else 0) z mem
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "cant write im val"

config :: Memory -> [Int] -> Int
config mem [m0,m1,m2,m3,m4] = last ot4
  where
    ot0 = runMem mem (m0:0:ot4) 0
    ot1 = runMem mem (m1:ot0)   0
    ot2 = runMem mem (m2:ot1)   0
    ot3 = runMem mem (m3:ot2)   0
    ot4 = runMem mem (m4:ot3)   0
config _ _ = undefined

-- 929800
part_1 :: Memory -> IO ()
part_1 mem = print $ maximum $ config mem <$> permutations [0..4]

-- 15432220
part_2 :: Memory -> IO ()
part_2 mem = print $ maximum $ config mem <$> permutations [5..9]

main :: IO ()
main = do
  print tests

  prog <- getLine
  part_1 (loadProg prog)
  part_2 (loadProg prog)
