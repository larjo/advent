module Main where

import Control.Applicative (liftA2)
import Data.List (inits, tails)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

allInRange :: [Int] -> Bool
allInRange =
  (all (between 1 3) <||> all (between (-3) (-1))) . diffs
  where
    between a b x = a <= x && x <= b
    (<||>) = liftA2 (||)

removeOne :: [a] -> [[a]]
removeOne = liftA2 (zipWith (++)) inits (tail . tails)

maxOneOutsideRange :: [Int] -> Bool
maxOneOutsideRange = any allInRange <$> removeOne

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr "Part 1: "
  print . length . filter allInRange . map (map read . words) . lines $ input

  putStr "Part 2: "
  print . length . filter maxOneOutsideRange . map (map read . words) . lines $ input
