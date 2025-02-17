module Main where

import Data.Char
import Test.HUnit
import Data.Function (on)

testDiskMap :: String
testDiskMap = "2333133121414131403"

expand :: String -> [Int]
expand diskMap = helper True (map digitToInt diskMap) (iterate (+ 1) 0)
  where
    helper True (b : bs) (i : is) = replicate b i ++ helper False bs is
    helper False (b : bs) is = replicate b (-1) ++ helper True bs is
    helper _ _ _ = []

-- go :: Int -> Int -> [Int] -> [Int] -> [Int]
-- go bi fi _ _ | bi < fi = []
-- go bi fi (b : bs) (f : fs) | f == -1 && b == -1 = go (bi - 1) (fi + 1) bs fs
-- go bi fi (b : bs) (f : fs) | f == -1 && b /= -1 = b : go (bi - 1) (fi + 1) bs fs
-- go bi fi (b : bs) (f : fs) | f /= -1 && b == -1 = f : go (bi - 1) (fi + 1) bs fs
-- go bi fi bs (f : fs)       | f /= -1 = f : go bi (fi + 1) bs fs
-- go _ _ _ _ = []

-- compact :: [Int] -> [Int]
-- compact layout = go (length layout) 1 (reverse layout) layout
-- 

-- f [8, -1, 2, 3, 4]
-- r [4,  3, 2,-1, 8]
--   [8, 4, 2, 3]
compact :: [Int] -> [Int]
compact l =
  go2 l (reverse l)
  where
    go2 (f : fs) (r : _) | f == -1 && r /= -1 = r : compact (init fs)
    go2 fs (r : _) | r == -1 = compact $ init fs
    go2 (f : fs) (r : _) | f /= -1 && r /= -1 = f : compact fs
    go2 [] [] = []
    go2 _ _ = fail ""

checkSum :: [Int] -> Int
checkSum  = sum . zipWith (*) indicies 
  where
    indicies = iterate (+ 1) 0

canon :: String -> [Int]
canon = map (\c -> if c == '.' then -1 else digitToInt c)

compareBlocks :: String -> String -> Bool
compareBlocks = (==) `on` canon

checkSumTests :: [Test]
checkSumTests =
  [ TestCase $ assertEqual "checkSum 1" 0 $ checkSum [],
    TestCase $ assertEqual "checkSum 2" 0 $ checkSum [2],
    TestCase $ assertEqual "checkSum 3" 3 $ checkSum [2, 3],
    TestCase $ assertEqual "checkSum 4" 11 $ checkSum [2, 3, 4]
  ]

compactTests :: [Test]
compactTests =
  [ TestCase $ assertEqual "compact 1" [] $ compact [],
    TestCase $ assertEqual "compact 2" [1] $ compact [1],
    TestCase $ assertEqual "compact 2" [1, 2] $ compact [1, 2],
    TestCase $ assertEqual "compact 2" [1, 2, 3] $ compact [1, 2, 3],
    TestCase $ assertEqual "compact 3" [2] $ compact [-1, -1, 2],
    TestCase $ assertEqual "compact 4" [3, 2] $ compact [-1, -1, 2, 3],
    TestCase $ assertEqual "compact 5" [2, 3] $ compact [-1, 3, -1, 2],
    TestCase $ assertEqual "compact 6" [2] $ compact [-1, 2],
    TestCase $ assertEqual "compact 7" [4, 2, 3] $ compact [-1, 2, 3, 4],
    TestCase $
      assertEqual
        "compact 9b"
        (canon "87733374465555666")
        (compact $ canon "...333.44.5555.6666.777.8"), -- ok
    TestCase $
      assertEqual
        "compact 9d"
        (canon "87733374465555666")
        (compact $ canon "8..333.44.5555.6666.777."), -- ok  
    TestCase $
      assertEqual
        "compact 9c"
        (canon "87733374465555666")
        (compact $ canon "87.333.44.5555.6666.77"), -- ok        
    TestCase $
      assertEqual
        "compact 10"
        (canon "00999111888287733374465555666")
        (compact $ canon "0099911188828..333.44.5555.6666.777."), -- ok
    TestCase $
      assertEqual
        "compact 11"
        (canon "00999111888287733374465555666")
        (compact $ canon "009991118882877333744.5555.6666"), -- 
    TestCase $
      assertEqual
        "compact 12"
        (canon "00999111888287733374465555666")
        (compact $ canon "00999111888287733374465555666"), -- 
    TestCase $
      assertEqual
        "compact 13"
        (canon "00999111888287733374465555666")
        (compact $ canon "00999111888287733374465555.666")
  ]

runTest :: IO ()
runTest = do
  _ <- runTestTT . TestList $ checkSumTests ++ compactTests
  return ()

main :: IO ()
main = do
  runTest
  print testDiskMap
  print . expand $ testDiskMap
  print . checkSum . compact . expand $ testDiskMap

  readFile "input.txt" >>= print . checkSum . compact . expand

-- 6639544530862 to high
