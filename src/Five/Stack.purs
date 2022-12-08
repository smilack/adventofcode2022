module AdventOfCode.Twenty22.Five.Stack
  ( Stack
  , empty
  , push
  , pop
  , peek
  ) where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

data Stack a = Stack (List a)

_show :: List String -> String
_show Nil = "(empty)"
_show (a : Nil) = a
_show (a : as) = a <> "->" <> _show as

instance Show (Stack String) where
  show (Stack a) = _show a
else instance Show a => Show (Stack a) where
  show (Stack a) = _show $ map show a

instance Eq a => Eq (Stack a) where
  eq (Stack a) (Stack b) = eq a b

empty :: forall a. Stack a
empty = Stack Nil

push :: forall a. a -> Stack a -> Stack a
push a (Stack as) = Stack $ a : as

pop :: forall a. Stack a -> Stack a
pop (Stack (_ : as)) = Stack as
pop (Stack Nil) = Stack Nil

peek :: forall a. Stack a -> Maybe a
peek (Stack (a : _)) = Just a
peek (Stack Nil) = Nothing