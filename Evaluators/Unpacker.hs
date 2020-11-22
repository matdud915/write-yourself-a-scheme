{-# LANGUAGE ExistentialQuantification #-}
module Evaluators.Unpacker where

import LispCore

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)