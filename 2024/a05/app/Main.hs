module Main where

import Data.List (intersect)
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

parseUpdates :: [String] -> [[Int]]
parseUpdates = map (map read . splitOn ",")

checkOrder :: [(Int, Int)] -> [Int] -> Bool
checkOrder orderings = checkPage
  where
    checkPage (h : t) = null (precedents `intersect` t) && checkPage t
      where
        precedents = map fst . filter ((== h) . snd) $ orderings
    checkPage [] = True

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (orderingsInput, updatesInput) = parts input
  let orderings = parseOrderings orderingsInput
  let updates = parseUpdates updatesInput

  putStr "Part 1: "
  print $ sum . map mid . filter (checkOrder orderings) $ updates
