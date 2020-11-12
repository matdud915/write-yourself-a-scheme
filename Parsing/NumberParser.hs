module NumberParser where
import Text.ParserCombinators.Parsec ( digit, many1, Parser )
import LispCore ( LispVal(Number) )

parseNumber :: Parser LispVal
parseNumber = do
  numberParser <- many1(digit) 
  return $ (Number . read) numberParser
