module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

numberParser :: Parser Int
numberParser = do
  a <- count' 1 3 digitChar
  return $ read a

data MulState = MulState
  { acc :: Int,
    enabled :: Bool
  }
  deriving (Show)

mulParser :: Parser Int
mulParser = do
  _ <- string "mul"
  _ <- char '('
  a <- numberParser
  _ <- char ','
  b <- numberParser
  _ <- char ')'
  return $ a * b

doParser :: MulState -> Parser MulState
doParser mulState = do
  _ <- string "do()"
  return mulState { enabled = True }

dontParser :: MulState -> Parser MulState
dontParser mulState = do
  _ <- string "don't()"
  return mulState { enabled = False }

mulStateParser :: MulState -> Parser MulState
mulStateParser mulState = do
  val <- mulParser
  if enabled mulState
    then return $ mulState {acc = acc mulState + val}
    else return mulState

parsePart1 :: Parser Int
parsePart1 = getParserState >>= go 0
  where
    go prod state =
      case stateInput state of
        [] -> return prod
        _ ->
          let (nextState, res) = runParser' mulParser state in
          case res of
            Right val -> go (val + prod) nextState
            Left _ -> go prod (state {stateInput = tail (stateInput state)})

parsePart2 :: Parser MulState
parsePart2 = getParserState >>= go (MulState 0 True)
  where
    go mulState state =
      case stateInput state of
        [] -> return mulState
        _ ->
          let (nextState, res) = runParser' (mulStateParser mulState <|> doParser mulState <|> dontParser mulState) state
           in case res of
                Right val -> go val nextState
                Left _ -> go mulState (state {stateInput = tail (stateInput state)})

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  parseTest parsePart1 input
  putStr "Part 2: "
  parseTest parsePart2 input
