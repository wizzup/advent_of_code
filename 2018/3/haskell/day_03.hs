{-# LANGUAGE RecordWildCards #-}

import Text.Read (readPrec)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (lift)
import qualified Text.ParserCombinators.ReadP as R

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import Data.Char (isDigit)

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = "A: " ++ (fst res) ++ "\nB: " ++ (snd res) ++ "\n"
  where res = solves cs
        cs = map read . lines $ xs

-- $setup
-- >>> c = "#2 @ 3,1: 2x2"
--
-- >>> c1 = read "#1 @ 1,3: 4x4" :: Claim
-- >>> c2 = read "#2 @ 3,1: 4x4" :: Claim
-- >>> c3 = read "#3 @ 5,5: 2x2" :: Claim

-- |
-- >>> solves [c1,c2,c3]
-- ("4","3")
--
solves :: [Claim] -> (String,String)
solves xs = (,) ansA ansB
  where ansA = show . M.size $ conflict
        ansB = show alone
        m = claimMap xs
        conflict = M.filter ((>1) . length) $ m
        conflictIds = S.fromList . concat . M.elems $ conflict
        allId = S.fromList . map cId $ xs
        alone = head . S.toList $ S.difference allId conflictIds

data Claim = Claim
  { cId     :: Int
  , cX      :: Int
  , cY      :: Int
  , cWidth  :: Int
  , cHeight :: Int
  } deriving Show

instance Read Claim where
  readPrec = lift readClaim

-- |
-- >>> read c :: Claim
-- Claim {cId = 2, cX = 3, cY = 1, cWidth = 2, cHeight = 2}
--
readClaim :: ReadP Claim
readClaim =
  let number :: ReadP Int
      number = read <$> R.munch1 isDigit
  in do
    i <- R.char '#' *> number
    x  <- R.string " @ " *> number
    y  <- R.char ',' *> number
    w  <- R.string ": " *> number
    h  <- R.char 'x' *> number
    return $ Claim i x y w h

type Pos = (Int,Int)

-- | Map of position of a claim
--
-- >>> getClaimMap $ read c
-- fromList [((3,1),[2]),((3,2),[2]),((4,1),[2]),((4,2),[2])]
--
getClaimMap :: Claim -> Map Pos [Int]
getClaimMap Claim {..} = M.fromList cs
  where cs = [ ((i,j), [cId])
             | i <- take cWidth [cX..]
             , j <- take cHeight [cY..]
             ]

-- | Map of all claims
--
claimMap :: [Claim] -> Map Pos [Int]
claimMap = (M.unionsWith (++)) . map getClaimMap
