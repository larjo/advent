module Main where

import Data.Functor ((<&>))
import Part1 qualified (run)
import Part2 qualified (run)

main :: IO ()
main = do
  rows <- readFile "assets/input.txt" <&> map splitInput . lines
  print $ Part1.run rows -- 251136060
  print $ Part2.run rows -- 249400220

splitInput :: String -> (String, Int)
splitInput row = (hand, bet)
  where
    hand = take 5 row
    bet = read $ drop 6 row
