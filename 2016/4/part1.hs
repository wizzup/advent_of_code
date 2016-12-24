{-# LANGUAGE NamedFieldPuns #-}

import Data.Function
import Data.List
import Data.List.Split

parseLine :: String -> Room
parseLine xs =  mkRoom (name, sid, chksum)
  where ss = splitOn "-" xs
        name = concat $ init ss
        xx = splitOn "[" $ last ss
        sid  = read $ head xx :: Int
        chksum = init $ last xx

common5 :: String -> String
common5 xs = take 5 ps
  where ss = group . sort $ xs
        cs = map length ss
        hs = map head ss
        -- ps = zipWith (curry fst) ss cs
        ps = map fst 
           . sortBy (flip compare `on` snd) 
           $ zip hs cs

data Room = R { name :: String
              , sid :: Int
              , chks :: String}
  deriving Show

mkRoom :: (String, Int, String) -> Room
mkRoom (n,i,s) = R { name = n, sid = i, chks = s}

check :: Room -> Bool
check R {name = n,chks = c} = common5 n == c

main :: IO ()
main = do
  ls <- map parseLine . lines <$> readFile "input"
  print . sum . map sid $ filter check ls
