module NumberParser where
import Text.ParserCombinators.Parsec hiding (spaces)
import LispCore
import Numeric
import Data.Char (digitToInt)

parseNumber :: Parser LispVal
parseNumber = do
  numberParser <- many1(digit) 
  return $ (Number . read) numberParser
