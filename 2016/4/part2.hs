{-# LANGUAGE NamedFieldPuns #-}

import Data.List
import Data.List.Split

parseLine :: String -> Room
parseLine xs =  mkRoom (name, sid, chksum)
  where ss = splitOn "-" xs
        name = unwords $ init ss
        xx = splitOn "[" $ last ss
        sid  = read $ head xx :: Int
        chksum = init $ last xx

data Room = R { name :: String
              , sid :: Int
              , chks :: String}
  deriving Show

mkRoom :: (String, Int, String) -> Room
mkRoom (n,i,s) = R { name = n, sid = i, chks = s}

rot :: Int -> Char -> Char
rot x c = if c `elem` as then z else c
  where as = "abcdefghijklmnopqrstuvwxyz"
        bs = concat $ repeat as
        z = head . drop x $ dropWhile (/= c) bs

decode :: Room -> (Int, String)
decode R {name = n,sid = s} = (s,dec)
  where dec = map (rot $ s `mod` 26) n

main :: IO ()
main = do
  ls <- map parseLine . lines <$> readFile "input"
  let ds = filter (isInfixOf "north" . snd) $ map decode ls
  print ds
