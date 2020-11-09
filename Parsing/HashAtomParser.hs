module HashAtomParser where
import Text.ParserCombinators.Parsec hiding (spaces)
import LispCore
import Flow
import Numeric
import Data.Char (digitToInt)

parseTrueLiteral :: Parser LispVal
parseTrueLiteral = do
  char 't'
  return $ Bool True

parseFalseLiteral :: Parser LispVal
parseFalseLiteral = do
  char 'f'
  return $ Bool False

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

parseHashDecimalWithDot :: Parser String
parseHashDecimalWithDot = do
  wholeNumberPart <- many digit
  separator <- optionMaybe (char '.')
  decimal <- case separator of
            Just dot -> do
              decimalPart <- many digit
              return $ (concat [wholeNumberPart, [dot], decimalPart])
            Nothing -> do return $ wholeNumberPart
  return decimal

parseHashDecimalWithoutDot :: Parser String
parseHashDecimalWithoutDot = do
  wholeNumberPart <- many1 digit
  return wholeNumberPart

parseHashDecimal :: Parser LispVal
parseHashDecimal = do
  char 'd'
  decimal <- parseHashDecimalWithDot 
  let parsedDecimal = readFloat decimal
  return parsedDecimal >>= \x -> x !! 0 |> fst |> Decimal |> return

parseHashNumber :: Parser LispVal
parseHashNumber = do
  num <- parseBinaryNumber <|> parseOctalNumber <|> parseHexNumber
  num |> read |> Number |> return

parseCharacter :: Parser LispVal
parseCharacter = do
  char '\\'
  character <- anyChar
  return $ Character character

parseHashAtom :: Parser LispVal
parseHashAtom = do
  parseTrueLiteral <|> parseFalseLiteral <|>
    parseCharacter <|> parseHashDecimal <|>
      parseHashNumber
