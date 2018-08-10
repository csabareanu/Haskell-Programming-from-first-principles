module Polymorphic where

-- all infix data constructors must start with ":" and all operators that start with ":" must be of infix type or data constructors.
-- The type constructor of functions (->) is the only infix type constructor that doesn't start with a colon.

data Product a b =
    a :&: b
    deriving (Eq, Show)

data List a = Nil | Cons a (List a)
