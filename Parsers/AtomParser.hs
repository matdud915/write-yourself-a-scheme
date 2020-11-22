module Parsers.AtomParser where

import LispCore (LispVal (Atom))
import Parsers.HashAtomParser (parseHashAtom)
import Text.ParserCombinators.Parsec
  ( Parser,
    digit,
    letter,
    many,
    oneOf,
    (<|>),
  )

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  case first of
    '#' -> parseHashAtom
    _ -> do
      rest <- many (letter <|> symbol <|> digit)
      let atom = first : rest
      return $ Atom atom
