module Main where

import Data.Maybe (mapMaybe)
import MyLib (extract)
import System.IO

main :: IO ()
main = do
  text <- readFile "assets/input.txt"
  let result = sum $ mapMaybe extract $ lines text
  putStrLn $ "The result is: " ++ show result
