module WritingInstances1 where

-- bc. no deriving clause for Eq or bc. no written instance for Trivial, testing for Equality, Trivial == Trivial will throw a type error.
-- data Trivial = Trivial

-- keep your typeclass instances for a type
-- in the same file as that type
-- we'll explain why later

data Trivial = Trivial'

-- declaration of a typeclass instance. Typeclass instances are how you tell Haskell how equality, stringification (Show), orderability (Ord), enumeration (Enum) or other typeclasses -- should work for a particular datatype.

-- Eq -> the typeclass the instance is providing
-- Trivial -> the type the instance is being provided for
-- ---> Implementing the Eq typeclass for the Trivial datatype.
-- where -> terminates the initial declaration and beginning of the instance. What follows are the methods implemented.


instance Eq Trivial where
    (==) Trivial' Trivial' = Tru