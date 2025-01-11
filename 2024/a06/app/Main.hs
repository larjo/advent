module Main where

import Data.Array (Array, Ix (range), array, bounds, (!))
import Data.List (nub, intersect)
import Data.VectorSpace ((^+^))
import Debug.Trace (traceShow)

type Index = (Int, Int)

type CharArray = Array Index Char
data Vec = Vec
  { pos :: Index,
    dir :: Index
  }
  deriving (Show, Eq)

charAt :: CharArray -> Vec -> Char
charAt labMap (Vec p _) = labMap ! p

stopped :: Vec -> Bool
stopped (Vec _ (0, 0)) = True
stopped _ = False

outside :: CharArray -> Vec -> Bool
outside labMap (Vec (x, y) _) =
  x < xMin || x > xMax || y < yMin || y > yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds labMap

rotateIndex :: Index -> Index
rotateIndex (x, y) = (-y, x)

rotate :: Vec -> Vec
rotate v = v { dir = rotateIndex (dir v) }

mkCharArray :: [String] -> CharArray
mkCharArray l =
  array ((0, 0), (xMax, yMax)) [((i, j), (l !! j) !! i) | i <- [0 .. xMax], j <- [0 .. yMax]]
  where
    xMax = length (head l) - 1
    yMax = length l - 1

startVec :: Char -> CharArray -> Vec
startVec c labMap =
  Vec startPos (0, -1)
  where
    startPos = head [i | i <- range (bounds labMap), labMap ! i == c]

forward :: Vec -> Vec
forward (Vec curPos curDir) = Vec (curPos ^+^ curDir) curDir

move :: CharArray -> Vec -> Maybe Vec
move labMap curVec
  | outside labMap nextVec = Nothing
  | charAt labMap nextVec == '#' = move labMap . rotate $ curVec
  | otherwise = Just nextVec
  where
    nextVec = forward curVec

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
  x : go (f x)
  where
    go (Just y) = iterateMaybe f y
    go Nothing = []

candidates :: CharArray -> Vec -> [Vec]
candidates labMap = drop 1 . iterateMaybe (move labMap) . rotate

isLoop :: CharArray -> Vec -> Bool
isLoop labMap start = start `elem` candidates labMap start

possibleObstruction :: CharArray -> [Vec] -> Bool
possibleObstruction labMap partHistory =
  not . null $ cs `intersect` partHistory -- && traceShow (forward startVec) True
  where
    start = head partHistory
    cs = candidates labMap start

test :: IO ()
test = do
  input <- readFile "test-input.txt"
  let labMap = mkCharArray . lines $ input
  let partHistory = [Vec {pos = (4,6), dir = (0,-1)},Vec {pos = (4,5), dir = (0,-1)},Vec {pos = (4,4), dir = (0,-1)},Vec {pos = (4,3), dir = (0,-1)},Vec {pos = (4,2), dir = (0,-1)},Vec {pos = (4,1), dir = (0,-1)},Vec {pos = (5,1), dir = (1,0)},Vec {pos = (6,1), dir = (1,0)},Vec {pos = (7,1), dir = (1,0)},Vec {pos = (8,1), dir = (1,0)},Vec {pos = (8,2), dir = (0,1)},Vec {pos = (8,3), dir = (0,1)},Vec {pos = (8,4), dir = (0,1)},Vec {pos = (8,5), dir = (0,1)},Vec {pos = (8,6), dir = (0,1)},Vec {pos = (7,6), dir = (-1,0)},Vec {pos = (6,6), dir = (-1,0)},Vec {pos = (5,6), dir = (-1,0)},Vec {pos = (4,6), dir = (-1,0)},Vec {pos = (3,6), dir = (-1,0)},Vec {pos = (2,6), dir = (-1,0)},Vec {pos = (2,5), dir = (0,-1)}]
  let start = head partHistory
  let cs = candidates labMap start
  let ok = not . null $ cs `intersect` partHistory
  print start
  print cs
  print ok

main :: IO ()
main = do
  input <- readFile "input.txt"
  let labMap = mkCharArray . lines $ input
  let start = startVec '^' labMap
  let path = iterateMaybe (move labMap) start
  putStr "Part 1: "
  print $ length . nub . map pos $ path

  let c = length $ filter (isLoop labMap) path
  putStrLn "Candidates"
  print c
-- 
--   putStr "Part 2: "
--   print $ length . filter (possibleObstruction labMap . reverse) . tail . inits $ history
  -- 503 is too low
