module LispCore where

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal 
        | Number Integer
        | String String
        | Bool Bool
        | Character Char
        | Decimal Float
        deriving Show
