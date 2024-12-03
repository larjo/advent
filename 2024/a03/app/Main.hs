module Main where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

numberParser :: Parser Int
numberParser = do
  a <- count' 1 3 digitChar
  return $ read a

mulParser :: Parser Int
mulParser = do
  _ <- string "mul"
  _ <- char '('
  a <- numberParser
  _ <- char ','
  b <- numberParser
  _ <- char ')'
  return $ a * b

findAllOccurrences :: Parser Int -> Parser Int
findAllOccurrences p = go 0
  where
    go acc = do
      end <- atEnd
      if end
        then return acc
        else do
          res <- optional (try p)
          case res of
            Just val -> go (val + acc)
            Nothing -> anySingle *> go acc

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = parseMaybe (findAllOccurrences mulParser) input
  mapM_ print s
