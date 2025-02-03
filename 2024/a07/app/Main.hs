module Main where

import Data.Either (fromRight)
import Data.Function (on)
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

type Op = Int -> Int -> Int

(|||) :: Op
(|||) a b = read $ a +++ b
  where
    (+++) = (++) `on` show

calc1 :: [Op] -> Int -> [Int] -> [Int]
calc1 ops n acc = [a `op` n | a <- acc, op <- ops]

calcs :: [Op] -> [Int] -> [Int]
calcs _ [] = []
calcs ops (a : ns) = foldr (calc1 ops) [a] . reverse $ ns

testEq :: [Op] -> Equation -> Maybe Int
testEq ops eq = find (== result eq) . calcs ops . numbers $ eq

main :: IO ()
main = do
  input <- readFile "input.txt"
  let res = parse inputParser "" input
  let eqs = fromRight [] res

  putStr "Part 1: "
  print $ sum . mapMaybe (testEq [(*), (+)]) $ eqs

  putStr "Part 2: "
  print $ sum . mapMaybe (testEq [(*), (+), (|||)]) $ eqs
