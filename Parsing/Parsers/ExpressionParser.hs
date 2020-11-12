module Parsers.ExpressionParser where

import Control.Monad (liftM)
import LispCore (LispVal (Atom, DottedList, List))
import Parsers.AtomParser (parseAtom)
import Parsers.NumberParser (parseNumber)
import Parsers.StringParser (parseString)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    endBy,
    sepBy,
    skipMany1,
    space,
    try,
    (<|>),
  )

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x
