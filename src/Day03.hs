{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Day03 where

import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Text.Printf

walk :: Char -> (Int, Int) -> (Int, Int)
walk '>' = first (+1)
walk '<' = first (subtract 1)
walk 'v' = second (+1)
walk '^' = second (subtract 1)
walk _ = id

split :: [a] -> ([a], [a])
split = flip go1 ([], [])
  where
    go1 [] (acc1, acc2) = (reverse acc1, reverse acc2)
    go1 (x:xs) (acc1, acc2) = go2 xs (acc2, x:acc1)
    go2 [] (acc1, acc2) = (reverse acc2, reverse acc1)
    go2 (x:xs) (acc1, acc2) = go1 xs (acc2, x:acc1)

day03 :: IO ()
day03 = do
  s <- readFile "data/day03.txt"
  let (s1, s2) = split s

  let deliver = scanl (flip walk) (0, 0)
  let tally = Map.size . Map.filter (>=1)
  let run =  foldr (flip (Map.insertWith (+)) 1) Map.empty . deliver

  printf "Part1: %s\n" (show . tally $ run s)
  printf "Part2: %s\n" (show . tally
                        $ Map.unionWith (+) (run s1) (run s2))
