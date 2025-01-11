module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.List (nub, intersect, inits)
import Data.VectorSpace ((^+^))
import Debug.Trace (traceShow)

type Index = (Int, Int)

type CharArray = Array Index Char
data Vec = Vec
  { pos :: Index,
    dir :: Index
  }
  deriving (Show, Eq)

data Guard = Guard
  { vec :: Vec,
    hist :: [Vec]
  }
  deriving (Show)

outside :: CharArray -> Index -> Bool
outside labMap (x, y) =
  x < xMin || x > xMax || y < yMin || y > yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds labMap

fix :: (Guard -> Guard) -> Guard -> Guard
fix f x =
  if (dir . vec $ x) == (0, 0)
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
  Guard startVec [startVec]
  where
    startPos = head [i | i <- range (bounds labMap), labMap ! i == c]
    startVec = Vec startPos (0, -1)

move :: CharArray -> Guard -> Guard
move labMap (Guard (Vec curPos curDir) curHist)
  | outside labMap nextPos = Guard (Vec curPos (0, 0)) curHist
  | labMap ! nextPos == '#' = move labMap $ Guard (Vec curPos (rotate curDir)) curHist
  | otherwise = Guard (Vec nextPos curDir) (Vec nextPos curDir : curHist)
  where
    nextPos = curPos ^+^ curDir

candidates :: CharArray -> Vec -> [Vec]
candidates labMap (Vec curPos curDir) =
  map (`Vec` candidateDir) . takeWhile isCandidate . drop 1 . iterate (^+^ candidateDir) $ curPos
  where
    isCandidate p = not (outside labMap p) && (labMap ! p /= '#')
    candidateDir = rotate curDir

forward :: Vec -> Vec
forward (Vec (x, y) (dx, dy)) = Vec (x + dx, y + dy) (dx, dy)

possibleObstruction :: CharArray -> [Vec] -> Bool
possibleObstruction labMap partHistory =
  (not . null $ cs `intersect` partHistory) && traceShow (forward startVec) True
  where
    startVec = head partHistory
    cs = candidates labMap startVec

test :: IO ()
test = do
  input <- readFile "test-input.txt"
  let labMap = mkCharArray . lines $ input
  let partHistory = [Vec {pos = (4,6), dir = (0,-1)},Vec {pos = (4,5), dir = (0,-1)},Vec {pos = (4,4), dir = (0,-1)},Vec {pos = (4,3), dir = (0,-1)},Vec {pos = (4,2), dir = (0,-1)},Vec {pos = (4,1), dir = (0,-1)},Vec {pos = (5,1), dir = (1,0)},Vec {pos = (6,1), dir = (1,0)},Vec {pos = (7,1), dir = (1,0)},Vec {pos = (8,1), dir = (1,0)},Vec {pos = (8,2), dir = (0,1)},Vec {pos = (8,3), dir = (0,1)},Vec {pos = (8,4), dir = (0,1)},Vec {pos = (8,5), dir = (0,1)},Vec {pos = (8,6), dir = (0,1)},Vec {pos = (7,6), dir = (-1,0)},Vec {pos = (6,6), dir = (-1,0)},Vec {pos = (5,6), dir = (-1,0)},Vec {pos = (4,6), dir = (-1,0)},Vec {pos = (3,6), dir = (-1,0)},Vec {pos = (2,6), dir = (-1,0)},Vec {pos = (2,5), dir = (0,-1)}]
  let startVec = head partHistory
  let cs = candidates labMap startVec
  let ok = not . null $ cs `intersect` partHistory
  print startVec
  print cs
  print ok

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let labMap = mkCharArray . lines $ input
  let start = guardStart '^' labMap

  putStr "Part 1: "
  let history = hist . fix (move labMap) $ start
  print $ length . nub . map pos $ history

  putStr "Part 2: "
  print $ length . filter (possibleObstruction labMap . reverse) . tail . inits . reverse $ history
  -- 503 is too low
