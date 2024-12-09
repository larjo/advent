module Main where

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
    go acc = do
      end <- atEnd
      if end
        then return acc
        else do
          res <- optional (try p)
          case res of
            Just val -> go (val + acc)
            Nothing -> anySingle *> go acc

test :: IO ()
test = do
  let inp = "mul(2,3)xmul(3,4)y"
  let state0 = initialState "nofile" inp :: State String Void
  let (state1, _res1) = runParser' mulParser state0
  let state2 = state1 { stateInput = drop 1 (stateInput state1) }
  let (state3, res3) = runParser' mulParser state2

  putStrLn $ "state3: " ++ show state3
  putStrLn $ "res3: " ++ show res3


-- manyMuls :: Parser [Int]
-- manyMuls = try mulParser <|> single alphaNumChar

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  parseTest (findAllOccurrences mulParser) input
