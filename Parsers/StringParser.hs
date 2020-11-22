module Parsers.StringParser where

import LispCore (LispVal (String))
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    many,
    many1,
    noneOf,
    oneOf,
    (<|>),
  )

parseEscapeCharacters :: Parser String
parseEscapeCharacters = do
  escape <- char '\\'
  character <- oneOf "\"nrt\\"
  return [escape, character]

parseNonEscapeCharacters :: Parser Char
parseNonEscapeCharacters = noneOf "\\\""

parseCharacter :: Parser String
parseCharacter = do
  parseEscapeCharacters <|> many1 parseNonEscapeCharacters

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many parseCharacter
  char '"'
  return $ String $ concat x
