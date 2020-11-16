module Main where

import Parsers.ExpressionParser (parseExpr)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import LispCore
import Evaluators.Eval

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
