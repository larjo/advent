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

turnParser :: Parser Turn
turnParser = Turn <$> anySingle <*> decimal

puzzleParser :: Parser [Turn]
puzzleParser = turnParser `sepBy` newline

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter (== 0) . scanl turn 50 . fromRight [] . parse puzzleParser "" $ input
