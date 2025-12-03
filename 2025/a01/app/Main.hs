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
turnCount (pos, zeroCount) (Turn 'L' dist) = ((pos - dist) `mod` 100, zeroCount + abs ((pos - dist) `div` 100))
turnCount (pos, zeroCount) (Turn 'R' dist) = ((pos + dist) `mod` 100, zeroCount + abs ((pos + dist) `div` 100))
turnCount _ _ = error "invalid direction"

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
