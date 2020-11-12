module Main where

import Parsers.ExpressionParser (parseExpr)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ readExpr expr
