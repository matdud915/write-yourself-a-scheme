module NumberParser where
import Text.ParserCombinators.Parsec hiding (spaces)
import LispCore
import Flow
import Numeric
import Data.Char (digitToInt)

parseBinaryNumber :: Parser String
parseBinaryNumber = do
  char 'b'
  binaryNumber <- many1 $ oneOf("01")
  return $ show $ foldl (\acc x -> acc*2 + ((toInteger . digitToInt) x)) 0 binaryNumber

parseOctalNumber :: Parser String
parseOctalNumber = do
  char 'o'
  octalNumber <- many1 $ oneOf("01234567")
  let parsedOctalNumber = readOct octalNumber
  return parsedOctalNumber >>= \x -> return $ show $ fst (x !! 0)

parseHexNumber :: Parser String
parseHexNumber = do
  char 'x'
  hexNumber <- many1 $ oneOf("0123456789abcdef")
  let parsedHexNumber = readHex hexNumber
  return parsedHexNumber >>= \x -> x !! 0 |> fst |> show |> return

parseNumberWithPrefix :: Parser String
parseNumberWithPrefix = do
  char '#'
  parseBinaryNumber <|> parseOctalNumber <|> parseHexNumber

parseNumber :: Parser LispVal
parseNumber = do
  numberParser <- many1(digit) <|> parseNumberWithPrefix
  return $ (Number . read) numberParser
