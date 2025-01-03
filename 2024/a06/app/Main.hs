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
outside labMap (x, y) =
  x < xMin || x > xMax || y < yMin || y > yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds labMap

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
guardStart c labMap =
  Guard startPos (0, -1) [startPos]
  where
    startPos = head [i | i <- range (bounds labMap), labMap ! i == c]

move :: CharArray -> Guard -> Guard
move labMap (Guard curPos curDir curHist)
  | outside labMap nextPos = Guard curPos (0, 0) curHist
  | labMap ! nextPos == '#' = move labMap $ Guard curPos (rotate curDir) curHist
  | otherwise = Guard nextPos curDir (nextPos : curHist)
  where
    nextPos = curPos ^+^ curDir
-- 
-- posibleObstruction :: CharArray -> Guard -> Bool
-- posibleObstruction labMap (Guard curPos curDir curHist) =
--   where
--     candidateDir = rotate curDir
--     candidates = takeWhile (not . outside labMap) . iterate (^+^ candidateDir) $ curPos
-- 
--     filter () curHist

candidates :: CharArray -> Guard -> [Index]
candidates labMap (Guard curPos curDir _curHist) =
  takeWhile isCandidate . drop 1 . iterate (^+^ candidateDir) $ curPos
  where
    isCandidate p = not (outside labMap p) && (labMap ! p /= '#')
    candidateDir = rotate curDir

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let labMap = mkCharArray . lines $ input
  let start = guardStart '^' labMap

  putStr "Part 1: "
  print $ length . nub . hist . fix (move labMap) $ start

  putStr "Part 2: "
  print $ candidates labMap start
  print $ candidates labMap (start {dir = (0, 1)})
