module Main where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, many, parse, sepBy)
import Text.Megaparsec.Char (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Equation = Equation
  { res :: Int,
    numbers :: [Int]
  }
  deriving (Show)

equationParser :: Parser Equation
equationParser = Equation <$> L.decimal <* string ": " <*> L.decimal `sepBy` char ' '

inputParser :: Parser [Equation]
inputParser = many (equationParser <* eol)

main :: IO ()
main = do
  input <- readFile "test-input.txt"
  let x = parse inputParser "" input
  case x of
    Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
    Right n -> mapM_ print n
