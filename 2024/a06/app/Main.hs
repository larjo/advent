module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.List (nub)
import Data.VectorSpace ((^+^))

type Index = (Int, Int)

type CharArray = Array Index Char

data Guard = Guard
  { pos :: Index,
    dir :: Index,
    hist :: [Index]
  }
  deriving (Show)

outside :: CharArray -> Index -> Bool
outside ca (x, y) =
  x < xMin || x > xMax || y < yMin || y > yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds ca

fix :: (Guard -> Guard) -> Guard -> Guard
fix f x =
  if dir x == (0, 0)
    then x
    else fix f (f x)

rotate :: Index -> Index
rotate (x, y) = (-y, x)

mkCharArray :: [String] -> CharArray
mkCharArray l =
  array ((0, 0), (xMax, yMax)) [((i, j), (l !! j) !! i) | i <- [0 .. xMax], j <- [0 .. yMax]]
  where
    xMax = length (head l) - 1
    yMax = length l - 1

guardStart :: Char -> CharArray -> Guard
guardStart c ca =
  Guard startPos (0, -1) [startPos]
  where
    startPos = head [i | i <- range (bounds ca), ca ! i == c]

move :: CharArray -> Guard -> Guard
move ca (Guard curPos curDir curHist)
  | outside ca nextPos = Guard curPos (0, 0) curHist
  | ca ! nextPos == '#' = move ca $ Guard curPos (rotate curDir) curHist
  | otherwise = Guard nextPos curDir (nextPos : curHist)
  where
    nextPos = curPos ^+^ curDir

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ca = mkCharArray . lines $ input
  let start = guardStart '^' ca
  putStr "Part 1 : "
  print $ length . nub . hist . fix (move ca) $ start
