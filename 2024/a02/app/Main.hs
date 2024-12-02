module Main where

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

diffs :: [Int] -> [Int]
diffs = map (uncurry (-)) . pairs

inRange :: Int -> Int -> [Int] -> Bool
inRange a b = all (\x -> a <= x && x <= b)

isOk :: [Int] -> Bool
isOk xs = inRange 1 3 xs || inRange (-3) (-1) xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter isOk . map (diffs . map read . words) . lines $ input
