module Main where

import Control.Monad.Except
import Evaluators.Eval
import LispCore
import LispError
import Parsers.ExpressionParser (parseExpr)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
