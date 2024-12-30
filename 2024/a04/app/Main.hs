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

countAllWords :: String -> CharArray -> Int
countAllWords word ca =
  sum . map (countWords word ca) . range . bounds $ ca

isXmas :: CharArray -> (Int, Int) -> Bool
isXmas ca (x, y) =
  checkMas (-1, -1) (1, 1) && checkMas (1, -1) (-1, 1)
  where
    checkMas startDelta delta =
      word == "MAS" || word == "SAM"
      where
        word = getWord ca 3 delta ((x, y) `addDelta` startDelta)
    addDelta (a, b) (c, d) = (a + c, b + d)

countAllXMas :: CharArray -> Int
countAllXMas ca =
  sum . map (bool 0 1 . isXmas ca) . range . bounds $ ca

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ca = mkCharArray . lines $ input
  putStr "Part 1: "
  print . countAllWords "XMAS" $ ca
  putStr "Part 2: "
  print . countAllXMas $ ca
