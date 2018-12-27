{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Day07 where

import           Control.Arrow
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Word
import           Debug.Trace
import           Lib
import           Text.ParserCombinators.ReadP hiding (get)
import           Text.Printf

import           Prelude                      hiding (read)

type Id = [Char]
data Input = Lit Word16 | Cnx Id deriving Show
type Output = Id
data Gate =
  Const Input Id
  | And Input Input Id
  | Or Input Input Id
  | Lshift Input Word16 Id
  | Rshift Input Word16 Id
  | Not Input Id
  deriving Show

gid :: ReadP Id
gid = many1 $ satisfy (\x -> x /= ' ' && not (isDigit x))

input :: ReadP Input
input = (Lit . fromIntegral  <$> digit) <++ (Cnx <$> gid)

line, parseConst, parseNot, parseBinop, parseShift :: ReadP Gate
line = choice [parseConst, parseNot, parseBinop, parseShift] <* eof

parseConst = Const <$> input <*> (string " -> " >> gid)
parseNot = Not <$> (string "NOT " >> input) <*> (string " -> " *> gid)
parseBinop = do
  l <- input
  op <- choice [
    string " AND " >> pure And,
    string " OR " >> pure Or ]
  r <- input
  d <- (string " -> " *> gid)
  pure $ op l r d

parseShift = do
  l <- input
  op <- choice [
    string " LSHIFT " >> pure Lshift,
    string " RSHIFT " >> pure Rshift ]
  r <- fromIntegral <$> digit
  d <- (string " -> " *> gid)
  pure $ op l r d

type R a = StateT (M.Map Id Gate) Maybe a
out :: Gate -> Id
out (Const _ i)    = i
out (Not _ i)      = i
out (And _ _ i)    = i
out (Or _ _ i)     = i
out (Lshift _ _ i) = i
out (Rshift _ _ i) = i


read :: Input -> R Word16
read (Lit i) = pure i
read (Cnx i) = do
  gets (M.lookup i) >>= \case
    Just gate -> do
      res <- eval gate
      modify (M.insert i (Const (Lit res) i))
      pure res
    Nothing -> lift Nothing

eval :: Gate -> R Word16
eval (Const v1 _)    = read v1
eval (Not v1 _)      = complement <$> read v1
eval (And v1 v2 _ )  = (.&.) <$> read v1 <*> read v2
eval (Or v1 v2 _ )   = (.|.) <$> read v1 <*> read v2
eval (Lshift v1 i _) = flip shiftL (fromIntegral i) <$> read v1
eval (Rshift v1 i _) = flip shiftR (fromIntegral i) <$> read v1


day07 :: IO ()
day07 = do
  i <- lines <$> readFile "data/day07.txt"
  let gates = fromJust . traverse (parse line) $ i
  let gatemap = M.fromList $ (out &&& id) <$> gates
  let res = M.lookup "a" gatemap >>= flip evalStateT gatemap . eval
  printf "Part1: %d\n" $ fromJust res
