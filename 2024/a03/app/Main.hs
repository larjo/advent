module Main where

import Data.Functor.Identity (Identity)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.State (initialState)

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
    go :: Int -> ParsecT Void String Identity Int
    go acc = do
      end <- atEnd
      if end
        then return acc
        else do
          res <- optional (try p)
          case res of
            Just val -> go (val + acc)
            Nothing -> anySingle *> go acc

parseAll :: Parser Int -> Parser Int
parseAll p = getParserState >>= go 0
  where
    go acc state =
      case stateInput state of
        [] -> return acc
        _ -> do
          let (nextState, res) = runParser' p state
          case res of
            Right val -> go (val + acc) nextState
            Left _ -> go acc (state {stateInput = tail (stateInput state)})

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  parseTest (parseAll mulParser) input
  parseTest (findAllOccurrences mulParser) input
