module Main where
import Control.Monad ( liftM )
import Text.ParserCombinators.Parsec
    ( char,
      digit,
      letter,
      oneOf,
      space,
      endBy,
      sepBy,
      skipMany1,
      (<|>),
      many,
      parse,
      try,
      Parser )
import System.Environment ( getArgs )
import NumberParser (parseNumber)
import LispCore ( LispVal(DottedList, List, Atom) )
import HashAtomParser (parseHashAtom)
import StringParser (parseString)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  case first of
    '#' -> parseHashAtom
    _ -> do
      rest <- many(letter <|> symbol <|> digit)
      let atom = first:rest
      return $ Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseAtom <|>
  parseString <|> parseNumber
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
