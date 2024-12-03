module Main where

import Control.Applicative (liftA2)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

allInRange :: [Int] -> Bool
allInRange = all (between 1 3) <||> all (between (-3) (-1))
  where
    between a b x = a <= x && x <= b
    (<||>) = liftA2 (||)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter allInRange . map (diffs . map read . words) . lines $ input
