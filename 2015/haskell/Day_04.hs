{-# LANGUAGE OverloadedStrings #-}

-- |
-- >>> :set -XOverloadedStrings

module Day_04
( partOne
, partTwo
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.MD5 as MD5

import Data.ByteString.Base16 (encode)
import Control.Monad

input :: ByteString
input = "bgvyzdsv"

leading :: Int -> ByteString
leading n = B.replicate n '0'

partOne :: IO ByteString
partOne = pure $ head $ compute 5

partTwo :: IO ByteString
partTwo = pure $ head $ compute 6

-- |
-- >>> hash "abcdef609043"
-- "000001dbbfa3a5c83a2d506429c7b00e"
-- >>> hash "pqrstuv1048970"
-- "000006136ef2ff3b291c85725f17325c"
hash :: ByteString -> ByteString
hash = encode . MD5.hash

compute :: Int -> [ByteString]
compute n = do
  num <- B.pack <$> map show ([0..]:: [Integer])
  let v = B.append input num
  let r = hash v
  guard $ leading n `B.isPrefixOf` r
  pure num
