module Main where

import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

data Turn = Turn {direction :: Char, distance :: Int} deriving (Show)

delta :: Char -> Int -> Int
delta dir dist =
  case dir of
    'L' -> -dist
    'R' -> dist
    _ -> error "invalid direction"

turn :: Int -> Turn -> Int
turn pos (Turn dir dist) = newPos `mod` 100
  where
    newPos = pos + delta dir dist

turnCount :: (Int, Int) -> Turn -> (Int, Int)
turnCount (pos, zeroCount) (Turn dir dist) =
  (newPos `mod` 100, zeroCount + abs (newPos `div` 100))
  where
    newPos = pos + delta dir dist

turnParser :: Parser Turn
turnParser = Turn <$> anySingle <*> decimal

puzzleParser :: Parser [Turn]
puzzleParser = turnParser `sepBy` newline

main :: IO ()
main = do
  input <- readFile "input.txt"
  let turns = fromRight [] . parse puzzleParser "" $ input

  putStrLn "Part 1:"
  print . length . filter (== 0) . scanl turn 50 $ turns

  putStrLn "Part 2:"
  print . snd . foldl turnCount (50, 0) $ turns
