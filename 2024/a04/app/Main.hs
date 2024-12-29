module Main where

import Data.Array (Array, array, (!))
type CharArray = Array (Int, Int) Char

mkCharArray :: [String] -> CharArray
mkCharArray l =
  array ((0, 0), (xMax, yMax)) [((i, j), (l !! j) !! i) | i <- [0 .. xMax], j <- [0 .. yMax]]
  where
    xMax = length (head l) - 1
    yMax = length l - 1

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ca = mkCharArray $ lines input
  print $ ca ! (0, 0)
  print $ ca ! (9, 0)
  print $ ca ! (0, 9)
  print $ ca ! (9, 9)
