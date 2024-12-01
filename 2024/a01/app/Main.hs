module Main where

import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

ints :: String -> ([Int], [Int])
ints =
  unzip . mapMaybe (getPair . map read . words) . lines
  where
    getPair [a, b] = Just (a, b)
    getPair _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      putStrLn $ "Processing: " ++ filename
      input <- readFile filename
      let (ints1, ints2) = ints input
      putStrLn "First"
      mapM_ print ints1

      putStrLn "Second"
      mapM_ print ints2

      putStrLn "done"
    _ -> putStrLn "One filename expected"
