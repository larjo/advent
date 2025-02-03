module Main where

import Data.Array (Array, Ix (range), array, bounds, (!), (//), listArray)
import Data.List (nub)

type Index = (Int, Int)

type CharArray = Array Index Char

mkCharArray :: [String] -> CharArray
mkCharArray l =
  listArray ((0, 0), (maxRow, maxCol)) . concat $ l
  where
    maxRow = length l - 1
    maxCol = length (head l) - 1

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let arr = mkCharArray . lines $ input
  putStr "Part 1: "
  print arr
