module Main where

import Control.Applicative (liftA2)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

inRange :: Int -> Int -> [Int] -> Bool
inRange a b = all (\x -> a <= x && x <= b)

isOk :: [Int] -> Bool
isOk = inRange 1 3 <||> inRange (-3) (-1)
  where
    (<||>) = liftA2 (||)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter isOk . map (diffs . map read . words) . lines $ input
