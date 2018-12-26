module Day06 where

import Data.Array
import Data.Maybe
import Data.Foldable
import Text.ParserCombinators.ReadP
import Text.Printf

import Lib

data Light = On | Off deriving (Eq, Show)
data Action = TurnOn | TurnOff | Toggle deriving Show
type Coord = (Int, Int)
type Ins = (Action, Coord, Coord)

action :: ReadP Action
action = choice [
    string "turn on" *> pure TurnOn
  , string "turn off" *> pure TurnOff
  , string "toggle" *> pure Toggle
  ]

line :: ReadP Ins
line = do
  a <- action <* skipSpaces
  x1 <- digit <* char ','
  y1 <- digit <* string " through "
  x2 <- digit <* char ','
  y2 <- digit <* eof
  return (a, (x1,y1) , (x2,y2))

day06 :: IO ()
day06 = do
  i <- lines <$> readFile "data/day06.txt"
  let ins = fromJust . traverse (parse line) $ i
  let lights = listArray ((0,0), (999, 999)) $ repeat Off
  let res = foldl' fondle lights ins
  printf "Part1: %d\n" (length . filter (==On) . elems $ res)

  let lights = listArray ((0,0), (999, 999)) $ repeat 0
  let res = foldl' fondle2 lights ins
  printf "Part1: %d\n" (sum res)

fondle :: Array Coord Light
  -> (Action, Coord, Coord) -> Array Coord Light
fondle arr (TurnOn, (x1,y1), (x2,y2)) = arr // [
  ((x, y), On) | x <- [x1..x2], y <- [y1..y2]]
fondle arr (TurnOff, (x1,y1), (x2,y2)) = arr // [
  ((x, y), Off) | x <- [x1..x2], y <- [y1..y2]]
fondle arr (Toggle, (x1,y1), (x2,y2)) = let
  targets = [(x, y)| x <- [x1..x2], y <- [y1..y2]]
  fetch ix = (ix, arr ! ix)
  current = fetch <$> targets
  toggle (coords, On) = (coords, Off)
  toggle (coords, Off) = (coords, On)
  next = toggle <$> current
  in arr // next


fondle2 :: Array Coord Int
  -> (Action, Coord, Coord) -> Array Coord Int
fondle2 arr (act, (x1,y1), (x2,y2)) = let
  targets = [(x, y)| x <- [x1..x2], y <- [y1..y2]]
  fetch ix = (ix, arr ! ix)
  foo (ix, v) = (ix, max 0 (v + act2val act))
  next = foo . fetch <$> targets
  in arr // next
  where act2val TurnOn = 1
        act2val TurnOff = -1
        act2val Toggle = 2
