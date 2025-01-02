module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.Bool (bool)
import Data.VectorSpace ((^+^))
import Debug.Trace (traceShow)

type Index = (Int, Int)

type CharArray = Array Index Char

data Guard = Guard
  { pos :: Index
  , dir :: Index
  } deriving (Show)

outside :: CharArray -> Index -> Bool
outside ca (x, y) =
  x < xMin || x > xMax || y < yMin || y > yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds ca

fix :: (Guard -> Maybe Guard) -> Guard -> Int
fix f x =
  case f x of
    Nothing -> 0
    Just x' -> 1 + fix f x'

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
  Guard startPos (0, -1)
  where
    startPos = head [i | i <- range (bounds ca), ca ! i == c]

move :: CharArray -> Guard -> Maybe Guard
move ca (Guard curPos curDir)
  | outside ca nextPos = Nothing
  | ca ! nextPos == '#' = Just $ Guard (curPos ^+^ rotate curDir) (rotate curDir)
  | otherwise = Just $ Guard nextPos curDir
  where
      nextPos = curPos ^+^ curDir

moveAll :: CharArray -> Guard -> Int
moveAll ca = fix (move ca)

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let ca = mkCharArray . lines $ input
  let start = guardStart '^' ca
  putStr "Part 1 (expects 41) : "
  print start
  print $ moveAll ca start
