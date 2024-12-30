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

inside :: CharArray -> (Int, Int) -> Bool
inside ca (x, y) =
  x >= xMin && x <= xMax && y >= yMin && y <= yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds ca

getWord :: CharArray -> Int -> (Int, Int) -> (Int, Int) -> String
getWord ca len (dx, dy) (x, y) =
  map (ca !) indices
  where
    indices = filter (inside ca) [(x + dx * n, y + dy * n) | n <- [0 .. len - 1]]

countWords :: String -> CharArray -> (Int, Int) -> Int
countWords word ca (x, y) =
  sum . map (bool 0 1 . checkWord) $ deltas
  where
    checkWord delta = getWord ca (length word) delta (x, y) == word
    deltas = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

countAll :: String -> CharArray -> Int
countAll word ca =
  sum . map (countWords word ca) . range . bounds $ ca

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  print . countAll "XMAS" . mkCharArray . lines $ input
