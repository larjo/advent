module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.Bool (bool)

type CharArray = Array (Int, Int) Char

mkCharArray :: [String] -> CharArray
mkCharArray l =
  array ((0, 0), (xMax, yMax)) [((i, j), (l !! j) !! i) | i <- [0 .. xMax], j <- [0 .. yMax]]
  where
    xMax = length (head l) - 1
    yMax = length l - 1

findStart :: Char -> CharArray -> (Int, Int)
findStart c ca =
  

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let ca = mkCharArray . lines $ input
  putStr "Part 1 (expects 41) : "
  print ca
