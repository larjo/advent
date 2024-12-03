module Main where

import Control.Applicative (liftA2)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

isOk :: [Int] -> Bool
isOk = allBetween 1 3 <||> allBetween (-3) (-1)
  where
    allBetween a b = all (\x -> a <= x && x <= b)
    (<||>) = liftA2 (||)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter isOk . map (diffs . map read . words) . lines $ input
