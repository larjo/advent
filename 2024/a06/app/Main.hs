module Main where

import Data.Array (Array, Ix (range), array, bounds, (!), (//))
import Data.List (nub)
import Data.VectorSpace ((^+^))

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
  | charAt labMap nextVec == '#' = Just . rotate $ curVec
  | otherwise = Just nextVec
  where
    nextVec = forward curVec

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
  x : go (f x)
  where
    go (Just y) = iterateMaybe f y
    go Nothing = []

checkLoop :: Eq a => [a] -> [a] -> Bool
checkLoop _ [] = False
checkLoop seen (x : xs)
    | x `elem` seen = True
    | otherwise = checkLoop (x : seen) xs

isLoop :: CharArray -> Vec -> Vec -> Bool
isLoop labMap start curr =
    allowed && checkLoop [] candidates
  where
    candidates = iterateMaybe (move labMap') start
    labMap' = labMap // [(pos curr, '#')]
    allowed = charAt labMap curr /= '^'

main :: IO ()
main = do
  input <- readFile "input.txt"
  let labMap = mkCharArray . lines $ input
  let start = startVec '^' labMap
  let path = iterateMaybe (move labMap) start
  putStr "Part 1: "
  print . length . nub . map pos $ path
  
  let obs = nub . map pos . filter (isLoop labMap start) $ path
  putStrLn "Candidates"
  print $ length obs
