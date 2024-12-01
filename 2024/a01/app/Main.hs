module Main where

import Control.Arrow (Arrow ((***)))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

locationLists :: String -> ([Int], [Int])
locationLists =
  unzip . mapMaybe (getPair . map read . words) . lines
  where
    getPair [a, b] = Just (a, b)
    getPair _ = Nothing

locationDiffs :: String -> Int
locationDiffs = sum . map (abs . uncurry (-)) . uncurry zip . (sort *** sort) . locationLists

similarity :: [Int] -> Int -> Int
similarity locationListRight location =
  location * count location locationListRight
  where
    count loc = length . filter (== loc)

similarityList :: [Int] -> [Int] -> Int
similarityList locationListRight = sum . map (similarity locationListRight)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", filename] -> do
      putStrLn $ "Part1: " ++ filename
      input <- readFile filename
      print $ locationDiffs input
    ["part2", filename] -> do
      putStrLn $ "Part2: " ++ filename
      input <- readFile filename
      let similarityScore = uncurry similarityList $ locationLists input
      print similarityScore
    _ -> putStrLn "One filename expected"
