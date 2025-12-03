module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

testPuzzle :: String
testPuzzle = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

data Direction = L | R deriving (Show)

data Turn = Turn {direction :: Direction, distance :: Int} deriving (Show)

turn :: Int -> Turn -> Int
turn pos (Turn L dist) = (pos - dist) `mod` 100
turn pos (Turn R dist) = (pos + dist) `mod` 100

directionParser :: Parser Direction
directionParser = (char 'L' $> L) <|> (char 'R' $> R)

turnParser :: Parser Turn
turnParser = do
  dir <- directionParser
  Turn dir <$> L.decimal

puzzleParser :: Parser [Turn]
puzzleParser = turnParser `sepBy` newline

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . length . filter (== 0) . scanl turn 50 . fromRight [] . parse puzzleParser "" $ input
