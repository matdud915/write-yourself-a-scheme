module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (digitToInt)
import Flow
import Numeric(readOct, readHex)
import NumberParser
import LispCore

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many(letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|>
  parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
