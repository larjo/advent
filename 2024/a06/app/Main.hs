module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.List (nub, intersect, inits)
import Data.VectorSpace ((^+^))

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
  map (`Vec` curDir) . takeWhile isCandidate . drop 1 . iterate (^+^ candidateDir) $ curPos
  where
    isCandidate p = not (outside labMap p) && (labMap ! p /= '#')
    candidateDir = rotate curDir

posibleObstruction :: CharArray -> [Vec] -> Int
posibleObstruction labMap history =
  length $ cs `intersect` history
  where
    startVec = head history
    cs = candidates labMap startVec

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let labMap = mkCharArray . lines $ input
  let start = guardStart '^' labMap

  putStr "Part 1: "
  let history = hist . fix (move labMap) $ start
  print $ length . nub . map pos $ history

  putStr "Part 2: "
  print history
  print $ map (posibleObstruction labMap) . tail . inits . reverse $ history