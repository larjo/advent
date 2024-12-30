module Main where

import Data.List.Extra (splitOn)

parts :: String -> ([String], [String])
parts =
  split . splitOn [""] . lines
  where
    split [p1, p2] = (p1, p2)
    split _ = error "Invalid input"


parseOrderings :: [String] -> [(Int, Int)]
parseOrderings = map (split . splitOn "|")
  where
    split [a, b] = (read a, read b)
    split _ = error "Invalid input"

parsePages :: [String] -> [[Int]]
parsePages = map (map read . splitOn ",")

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let (orderings, pages) = parts input

  putStr "orderings "
  print $ parseOrderings orderings

  putStr "pages "
  print $ parsePages pages
