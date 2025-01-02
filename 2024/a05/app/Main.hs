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

precedents :: [(Int, Int)] -> Int -> [Int]
precedents orderings page = map fst . filter ((== page) . snd) $ orderings

checkOrder :: [(Int, Int)] -> [Int] -> Bool
checkOrder orderings = checkPage
  where
    checkPage (page : pages) = null (precedents orderings page `intersect` pages) && checkPage pages
    checkPage [] = True

mid :: [a] -> a
mid l = mid' l l
  where
    mid' (_ : xs) (_ : _ : cs) = mid' xs cs
    mid' (x : _) [_] = x
    mid' _ [] = error "Only odd lengths are supported"
    mid' [] _ = error "Cannot happen"

pairs :: (a -> a -> b) -> [a] -> [b]
pairs f xs = zipWith f xs (tail xs)

fix :: (Eq t) => (t -> t) -> t -> t
fix f x =
  if x == x' then x else fix f x'
  where
    x' = f x

swapUpdate :: (Eq a) => [(a, a)] -> [a] -> [a]
swapUpdate orderings = swap
  where
    swap (a : b : xs)
      | (b, a) `elem` orderings = b : a : xs
      | otherwise = a : swap (b : xs)
    swap xs = xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (orderingsInput, updatesInput) = parts input
  let orderings = parseOrderings orderingsInput
  let updates = parseUpdates updatesInput

  putStr "Part 1: "
  print $ sum . map mid . filter (checkOrder orderings) $ updates

  putStr "Part 2: "
  print $ sum . map (mid . fix (swapUpdate orderings)) . filter (not . checkOrder orderings) $ updates
