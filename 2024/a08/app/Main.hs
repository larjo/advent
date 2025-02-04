module Main where

import Data.Array (Array, Ix (range), array, bounds, elems, listArray, assocs, (!), (//))
import Data.Function (on)
import Data.List (group, groupBy, nub)

type Index = (Int, Int)

type Element = (Index, Char)

type CharArray = Array Index Char

mkCharArray :: [String] -> CharArray
mkCharArray l =
  listArray ((0, 0), (maxRow, maxCol)) . concat $ l
  where
    maxRow = length l - 1
    maxCol = length (head l) - 1

extract :: [Element] -> [[Element]]
extract = groupBy ((==) `on` snd) . filter ((/=) '.' . snd)

pairs :: [Element] -> [(Index, Index)]
pairs elements = [ (a, b) | a <- indicies, b <- indicies, a < b]
  where
    indicies = map fst elements

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let arr = mkCharArray . lines $ input
  let grps = extract . assocs $ arr
  let grp1 = grps !! 0
  mapM_ print $ pairs grp1 
  putStr "Part 1: "
  mapM_ print grps
