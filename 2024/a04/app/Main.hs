module Main where

import Data.Array (Array, array, (!))

type CharArray = Array (Int, Int) Char

mkCharArray :: [String] -> CharArray
mkCharArray l =
  array ((0, 0), (xMax, yMax)) [((i, j), (l !! j) !! i) | i <- [0 .. xMax], j <- [0 .. yMax]]
  where
    xMax = length (head l) - 1
    yMax = length l - 1

getRange :: CharArray -> Int -> Int -> Int -> Int -> Int -> String
getRange ca count incX incY x y =
  map (ca !) indices
  where
    indices = [(x + incX * n, y + incY * n) | n <- [0 .. count - 1]]

checkXmas :: CharArray -> (Int, Int) -> Bool
checkXmas ca (x, y) =
  diag == "XMAS" || hor == "XMAS" || vert == "XMAS" || hor == "SAMX" || vert == "SAMX" || diag == "SAMX"
  where
    hor = getRange ca 4 1 0 x y
    vert = getRange ca 4 1 0 x y
    diag = getRange ca 4 1 1 x y

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ca = mkCharArray $ lines input
  print $ checkXmas ca (1, 2)
