module Day02 where

import Control.Applicative
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Text.Printf

import Lib (digit, parse)

parser :: ReadP (Int, Int, Int)
parser = do
  [x, y, z] <- sepBy1 digit (char 'x') <* eof
  return (x, y, z)

sideBy op (x, y, z) = [x `op` y, x `op` z, y `op` z]

paper dim =
  let sides = sideBy (*) dim
  in (sum sides * 2) + minimum sides

ribbon dim@(x, y, z) =
  let sides = sideBy (+) dim
  in (minimum sides) * 2 + (x * y * z)

day02 = do
  input <- fromJust . traverse (parse parser) . lines <$> readFile "data/day02.txt"
  printf "Part1: %d\n" (sum $ paper <$> input)
  printf "Part1: %d\n" (sum $ ribbon <$> input)
