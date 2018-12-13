module Day01 where

import Control.Applicative
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Text.Printf

import Lib (parse)

parser :: ReadP [Int]
parser = many1 ((char ')' >> pure (-1)) <|> (char '(' >> pure 1)) <* eof

day01 = do
  input <- fromJust . parse parser <$> readFile "data/day01.txt"
  printf "Part1: %d\n" (sum input)
  printf "Part1: %d\n" $ length . takeWhile (>=0) $ scanl (+) 0 input
