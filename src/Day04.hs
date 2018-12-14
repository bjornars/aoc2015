module Day04 where

import Data.Char (ord)
import Data.List (findIndex)
import Data.Maybe (fromJust)

import Data.Word (Word8)
import Crypto.Hash.MD5 (hash)
import Data.ByteString (pack, unpack)
import Text.Printf (printf)

toHex :: Word8 -> String
toHex w = let x = ['0'..'9'] <> "abcdef"
              (w1, w2) = w `divMod` 16
          in [x !! fromIntegral w1, x !! fromIntegral w2]

hash' :: String -> String
hash' = concatMap toHex . unpack . hash . pack . (fromIntegral . ord <$>)

mine :: String -> Int -> String
mine seed = hash' . (seed <>) .  show

zeroes :: Int -> [String] -> Int
zeroes n = fromJust . findIndex ((== replicate n '0') .  take n)

day04 :: IO ()
day04 = do
  let key = "iwrupvqb"
  let candidates = mine key <$> [0..]
  printf "Part1: %d\n" $ zeroes 5 candidates
  printf "Part2: %d\n" $ zeroes 6 candidates
