{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Char
import Text.ParserCombinators.ReadP

(>:>) = flip $ (.) . (.)

digit :: ReadP Int
digit = read <$> many1 (satisfy isDigit)

parse :: ReadP a -> String -> Maybe a
parse  = readP_to_S >:> \case
  [(res, "")] -> Just res
  _ -> Nothing
