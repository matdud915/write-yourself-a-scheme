{-# LANGUAGE ExistentialQuantification #-}
module Evaluators.Unpacker where

import LispCore
import LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)