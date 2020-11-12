module Parsers.NumberParser where

import LispCore (LispVal (Number))
import Text.ParserCombinators.Parsec (Parser, digit, many1)

parseNumber :: Parser LispVal
parseNumber = do
  numberParser <- many1 digit
  return $ (Number . read) numberParser
