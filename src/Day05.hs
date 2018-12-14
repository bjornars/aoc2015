module Day05 where

import Data.Semigroup (All(All), getAll)
import Data.List (filter, group, isInfixOf)
import Text.Printf (printf)

vowely, doubly, proper, doubly', repeaty :: String -> Bool
vowely = (>=3) . length. filter (`elem` "aeiou")
doubly = any ((>1).length) . group
proper = not . flip any ["ab", "cd", "pq", "xy"] . flip isInfixOf

doubly' (x: y: xs) | [x,y] `isInfixOf` xs = True
doubly' (_: xs) = doubly' xs
doubly' [] = False

repeaty (x: _: z: _) | x == z = True
repeaty (_: xs) = repeaty xs
repeaty [] = False

nice, nicer :: String -> Bool
nice = checky [vowely, doubly, proper]
nicer = checky [doubly', repeaty]

checky :: [a -> Bool] -> a -> Bool
checky = fmap getAll . foldMap (fmap All)

day05 :: IO ()
day05 = do
  l <- lines <$> readFile "data/day05.txt"
  printf "Part1: %d\n" . length . filter nice $ l
  printf "Part2: %d\n" . length . filter nicer $ l
