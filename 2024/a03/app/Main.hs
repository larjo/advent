module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void ( Void )

type Parser = Parsec Void String

helloParser :: Parser String
helloParser = string "hello"

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


-- Modified findAllOccurrences function
findAllOccurrences :: Parser a -> Parser [a]
findAllOccurrences p = go []
  where
    go acc = do
      end <- atEnd
      if end
        then return (reverse acc)
        else do
          res <- optional (try p)
          case res of
            Just val -> go (val : acc)
            Nothing  -> anySingle *> go acc

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn input
    let a = parseMaybe (findAllOccurrences mulParser) input
    print $ sum <$> a

