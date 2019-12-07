---------------------------------------------------------------------
-- https://adventofcode.com/2019/day/7
-- hacked the simulator from day 5
-- WARNING : only run work on my input file
---------------------------------------------------------------------

import Data.List.Extra

import           Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (readPrec)
import Data.Char
import Control.Monad
import qualified Data.IntMap as M
import Text.Printf

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

stepR :: ReadS Operation
stepR = R.readP_to_S readStep

type Code = [Operation]

readCode :: ReadP Code
readCode = R.many (readStep R.+++ readOper)

type Memory = M.IntMap Int

-- HACK: mem[8] = mode (0..4), m[9] = pipeline i/o
-- WARNING : only run work on my input file
loadProg' :: (Int,Int) -> String -> Memory
loadProg' (md,ip) ss
  = M.adjust (const md) 8
  $ M.adjust (const ip) 9 org
  where
    org = M.fromList . zip [0..] . map read . splitOn "," $ ss

memToProg' :: Int -> Memory -> String
memToProg' n = intercalate "," . map show . drop n . M.elems

data Computer
  = Computer {
    cMem :: Memory,
    cPC :: Int,
    cIn :: Int,
    cOut :: Int,
    isHalt :: Bool
  }

instance Show Computer where
  show (Computer mem pc _ o _)
    = printf "pc: %4i out: %4i " pc o
    <> printf "op: %-15s" op <> " "
    <> "mem@pc: "
    <> mem'
    where
      mem' = take 25 ( memToProg' pc mem)
      op = show . fst . head $ R.readP_to_S readOper mem'

mkComputer' :: (Int,Int) -> String -> Computer
mkComputer' (md,ip) prog = Computer (loadProg' (md,ip) prog) 0 0 0 False

step :: Computer -> Computer
step com@(Computer mem pc i _ h)
  -- | null parse     = com {isHalt = True}
  | null parse     = error $ "null parse:"
                  <> show pc <> ":"
                  <> show ( take 20 next)
  | otherwise
    = case op of
        Add m x y z -> doAdd m x y z
        Mul m x y z -> doMul m x y z
        Input _ _   -> com {cPC = pc + 2}
        Output m x  -> doOutput m x
        Halt        -> com {isHalt = True}
        Jnz m x y   -> doJnz m x y
        Jz m x y    -> doJz m x y
        Lt m x y z  -> doLt m x y z
        Eq m x y z  -> doEq m x y z
  where
    next = intercalate "," . map show . drop pc $ M.elems mem
    parse = stepR . intercalate "," . map show . drop pc $ M.elems mem
    op = fst . head $ parse
    fnd k = M.findWithDefault 0 k mem
    doOutput m x = Computer mem (pc + 2) i opt h
      where
        opt = case m of
                M000 -> fnd x
                M001 -> x
                _    -> error "too many params"
    doAdd m x y z = com {cMem = mop, cPC = pc + 4}
      where
        mop = case m of
                M000 -> M.adjust (const $ fnd x + fnd y) z mem
                M001 -> M.adjust (const $     x + fnd y) z mem
                M010 -> M.adjust (const $ fnd x +     y) z mem
                M011 -> M.adjust (const $     x +     y) z mem
                _    -> error "cant write im val"
    doMul m x y z = com {cMem = mop, cPC = pc + 4}
      where
        mop = case m of
                M000 -> M.adjust (const $ fnd x * fnd y) z mem
                M001 -> M.adjust (const $     x * fnd y) z mem
                M010 -> M.adjust (const $ fnd x *     y) z mem
                M011 -> M.adjust (const $     x *     y) z mem
                _    -> error "cant write im val"
    doEq m x y z = com {cMem = mop, cPC = pc + 4}
      where
        mop = M.adjust (const $ if x' == y' then 1 else 0) z mem
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "cant write im val"
    doLt m x y z = com {cMem = mop, cPC = pc + 4}
      where
        mop = M.adjust (const $ if x' < y' then 1 else 0) z mem
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "cant write im val"
    doJnz m x y = com {cPC = tgt}
      where
        tgt = if x' /= 0 then y' else pc + 3
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "too many params"
    doJz m x y = com {cPC = tgt}
      where
        tgt = if x' == 0 then y' else pc + 3
        (x',y') = case m of
                    M000 -> (fnd x, fnd y)
                    M001 -> (    x, fnd y)
                    M010 -> (fnd x,     y)
                    M011 -> (    x,     y)
                    _    -> error "too many params"

runTillHalts' :: (Int,Int) -> String -> [Computer]
runTillHalts' (md,inp) prog = takeWhile (not . isHalt) $ iterate step $ mkComputer' (md,inp) prog

runForOutput :: (Int,Int) -> String -> Int
runForOutput = ((cOut .) last .) .  runTillHalts'

config_1 :: String -> [Int] -> Int
config_1 prog [m0,m1,m2,m3,m4] = out4
  where
    out0 = runForOutput (m0, 0)    prog
    out1 = runForOutput (m1, out0) prog
    out2 = runForOutput (m2, out1) prog
    out3 = runForOutput (m3, out2) prog
    out4 = runForOutput (m4, out3) prog
config_1 _ _ = undefined

-- 929800
part_1 :: String -> IO ()
part_1 prog = print $ maximum $ config_1 prog <$> permutations [0..4]

main :: IO ()
main = do
  prog <- getLine
  part_1 prog
