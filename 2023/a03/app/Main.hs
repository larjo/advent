module Main where

import Data.Functor ((<&>))
import Part1 qualified (run)
import Part2 qualified (run)

main :: IO ()
main = do
  rows <- readFile "assets/input.txt" <&> lines
  print $ Part1.run rows
  print $ Part2.run rows
