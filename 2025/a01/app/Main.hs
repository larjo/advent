module Main where

import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

data Turn = Turn {direction :: Char, distance :: Int} deriving (Show)

turn :: Int -> Turn -> Int
turn pos (Turn 'L' dist) = (pos - dist) `mod` 100
turn pos (Turn 'R' dist) = (pos + dist) `mod` 100
turn _ _ = error "invalid direction"

turnCount :: (Int, Int) -> Turn -> (Int, Int)
turnCount (pos, zeros) (Turn 'L' dist) = ((pos - dist) `mod` 100, zeros + abs ((pos - dist) `div` 100))
turnCount (pos, zeros) (Turn 'R' dist) = ((pos + dist) `mod` 100, zeros + abs ((pos + dist) `div` 100))
turnCount _ _ = error "invalid direction"

turnParser :: Parser Turn
turnParser = Turn <$> anySingle <*> decimal

puzzleParser :: Parser [Turn]
puzzleParser = turnParser `sepBy` newline

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  print . length . filter (== 0) . scanl turn 50 . fromRight [] . parse puzzleParser "" $ input
  putStrLn "Part 2:"
  print . snd . foldl turnCount (50, 0) . fromRight [] . parse puzzleParser "" $ input
