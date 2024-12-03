module Main where

import Data.List (inits, tails)
import Control.Applicative (liftA2)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

between :: (Ord a) => a -> a -> a -> Bool
between a b x = a <= x && x <= b

(<||>) :: (Applicative f) => (f a -> Bool) -> (f a -> Bool) -> f a -> Bool
(<||>) = liftA2 (||)

allInRange :: [Int] -> Bool
allInRange = (all (between 1 3) <||> all (between (-3) (-1))) . diffs

removeOne :: [a] -> [[a]]
removeOne = liftA2 (zipWith (++)) inits (tail . tails)

maxOneOutsideRange :: [Int] -> Bool
maxOneOutsideRange ints =
  allInRange ints || any allInRange (removeOne ints)

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr "Part 1: "
  print . length . filter allInRange . map (map read . words) . lines $ input

  putStr "Part 2: "
  print . length . filter maxOneOutsideRange . map (map read . words) . lines $ input
