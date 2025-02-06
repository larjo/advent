module Main where

import Data.Char (digitToInt)

testDiskMap :: String
testDiskMap = "2333133121414131402"

expand :: String -> [Int]
expand diskMap = go True (map digitToInt diskMap) (iterate (+ 1) 0)
  where
    go True (b : bs) (i : is) = replicate b i ++ go False bs is
    go False (b : bs) is = replicate b (-1) ++ go True bs is
    go _ _ _ = []

compact :: [Int] -> [Int]
compact layout = go (length layout) 1 (reverse layout) layout
  where
    go :: Int -> Int -> [Int] -> [Int] -> [Int]
    go bi fi _ _ | bi < fi = []
    go bi fi (b : bs) (f : fs) | f == -1 && b == -1 = go (bi - 1) (fi + 1) bs fs
    go bi fi (b : bs) (f : fs) | f == -1 && b /= -1 = b : go (bi - 1) (fi + 1) bs fs
    go bi fi (b : bs) (f : fs) | f /= -1 && b == -1 = f : go (bi - 1) (fi + 1) bs fs
    go bi fi bs (f : fs) | f /= -1 = f : go bi (fi + 1) bs fs
    go _ _ _ _ = []

checkSum :: [Int] -> Int
checkSum layOut = sum $ zipWith (*) indicies blocks
  where
    indicies = iterate (+ 1) 0
    blocks = layOut

main :: IO ()
main = do
  print testDiskMap
  print . expand $ testDiskMap
  print . checkSum . compact . expand $ testDiskMap

  readFile "input.txt" >>= print . checkSum . compact . expand

-- 6639544530862 to high
