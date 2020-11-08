module StringParser where
import Text.ParserCombinators.Parsec hiding (spaces)
import LispCore

parseEscapeCharacters :: Parser String
parseEscapeCharacters = do
  escape <- char '\\'
  character <- oneOf ("\"nrt\\")
  return [escape, character]

parseNonEscapeCharacters :: Parser Char
parseNonEscapeCharacters = noneOf ("\\\"")

parseCharacter :: Parser String
parseCharacter = do
  parseEscapeCharacters <|> many1 parseNonEscapeCharacters

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many parseCharacter
  char '"'
  return $ String $ concat x
