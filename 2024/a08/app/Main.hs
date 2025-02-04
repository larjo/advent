module Main where

import Data.Array (Array, assocs, bounds, listArray)
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import Data.VectorSpace ((^+^), (^-^))

type Index = (Int, Int)

type Element = (Index, Char)

type Map = Array Index Char

mkMap :: [String] -> Map
mkMap l = listArray ((0, 0), (maxRow, maxCol)) . concat $ l
  where
    maxRow = length l - 1
    maxCol = length (head l) - 1

inside :: Map -> Index -> Bool
inside arr (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
  where
    ((xMin, yMin), (xMax, yMax)) = bounds arr

extract :: [Element] -> [[Index]]
extract = map (map fst) . groupBy ((==) `on` snd) . sortOn snd . filter ((/=) '.' . snd)

pairs :: [Index] -> [(Index, Index)]
pairs indicies = [(a, b) | a <- indicies, b <- indicies, a < b]

antiNodes :: (Index, Index) -> [Index]
antiNodes (a, b) = [a ^-^ diff, b ^+^ diff]
  where
    diff = b ^-^ a

genNodes :: (Index, Index) -> [Index]
genNodes (a, b) = iterate (^+^ diff) b
  where
    diff = b ^-^ a

genBoth :: (Index -> Bool) -> (Index, Index) -> [Index]
genBoth f (a, b) = takeWhile f a2b ++ takeWhile f b2a
  where
    a2b = genNodes (a, b)
    b2a = genNodes (b, a)

main :: IO ()
main = do
  arr <- mkMap . lines <$> readFile "input.txt"

  putStr "Part 1: "
  print . length . nub . concatMap (concatMap (filter (inside arr) . antiNodes) . pairs) . extract . assocs $ arr

  putStr "Part 2: "
  print . length . nub . concatMap (concatMap (genBoth (inside arr)) . pairs) . extract . assocs $ arr
