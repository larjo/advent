module Main where

import Data.Char (digitToInt, intToDigit)

testDiskMap :: String
testDiskMap = "2333133121414131402"

testLayout :: String
testLayout = "00...111...2...333.44.5555.6666.777.888899"

testCompacted :: String
testCompacted = "0099811188827773336446555566.............."

expand :: String -> String
expand diskMap = go True (map digitToInt diskMap) (iterate (+ 1) 0)
  where
    go True (b : bs) (i : is) = replicate b (intToDigit i) ++ go False bs is
    go False (b : bs) is = replicate b '.' ++ go True bs is
    go _ _ _ = ""

compact :: String -> String
compact layout = ""

checkSum :: String -> Int
checkSum layOut = sum $ zipWith (*) indicies blocks
  where
    indicies = iterate (+ 1) 0
    blocks = map digitToInt . takeWhile (/= '.') $ layOut

main :: IO ()
main = do
  print testDiskMap
  print . expand $ testDiskMap
  print . checkSum . compact . expand $ testDiskMap
