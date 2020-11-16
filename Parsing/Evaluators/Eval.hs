module Evaluators.Eval where

import LispCore

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args =
  case args of
    [x] -> applyUnary func x
    l@(_ : _) -> applyBinary func l

applyBinary :: String -> [LispVal] -> LispVal
applyBinary func args = maybe (Bool False) ($ args) $ lookup func primitives

applyUnary :: String -> LispVal -> LispVal
applyUnary func operand = maybe (Bool False) ($ operand) $ lookup func unaryPrimitives

unaryPrimitives :: [(String, LispVal -> LispVal)]
unaryPrimitives = [("number?", isNumber)]

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

-- unpackNum (String s) =
--   let parsed = reads s :: [(Integer, String)]
--    in if null parsed
--         then 0
--         else fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0