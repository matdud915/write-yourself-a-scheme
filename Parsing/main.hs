module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import NumberParser (parseNumber)
import LispCore
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

parseExpr :: Parser LispVal
parseExpr =  parseAtom <|>
  parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
