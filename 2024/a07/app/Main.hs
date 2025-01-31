module Main where

import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parse, sepBy)
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Equation = Equation
  { result :: Int,
    numbers :: [Int]
  }
  deriving (Show)

numberListParser :: Parser [Int]
numberListParser = L.decimal `sepBy` char ' '

equationParser :: Parser Equation
equationParser = Equation <$> L.decimal <* string ": " <*> numberListParser

inputParser :: Parser [Equation]
inputParser = many (equationParser <* eol)

calc1 :: Int -> [Int] -> [Int]
calc1 n acc = [op a n | a <- acc, op <- [(*), (+)]]

calcs :: [Int] -> [Int]
calcs [] = []
calcs (a : ns) = foldr calc1 [a] . reverse $ ns

testEq :: Equation -> Maybe Int
testEq eq = find (== result eq) . calcs . numbers $ eq

main :: IO ()
main = do
  input <- readFile "input.txt"
  let res = parse inputParser "" input
  let eqs = fromRight [] res
  putStr "Part 1: "
  print $ sum . mapMaybe testEq $ eqs
