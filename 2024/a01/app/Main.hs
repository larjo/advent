module Main where

import Control.Arrow (Arrow ((***)))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

intLists :: String -> ([Int], [Int])
intLists =
  unzip . mapMaybe (getPair . map read . words) . lines
  where
    getPair [a, b] = Just (a, b)
    getPair _ = Nothing

sumDiffs :: String -> Int
sumDiffs = sum . map (abs . uncurry (-)) . uncurry zip . (sort *** sort) . intLists

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      putStrLn $ "Processing: " ++ filename
      input <- readFile filename

      putStrLn "sum diffs"
      print $ sumDiffs input

      putStrLn "done"
    _ -> putStrLn "One filename expected"
