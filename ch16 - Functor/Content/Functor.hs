module Functor where

---------------------
-- In Logic
---------------------

-- Term created by Rudolf Carnap in the '30s for describing grammatical function words and logical operations over sentences or phrases.
-- Are combinators and take a sentence or word as input and produce a sentence or word as output with some logical operation applied.
-- Ex. Negation - Takes a sentence A, and produces the negated version !A as an output
--                It lifts the concept of negation to the whole sentence without changing it's internal structure;

-------------------------
-- In Haskell
-------------------------

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- A way of applying a function over a structure without altering it. The function gets applied to the value inside it, leaving the structure unchanged
-- Ex. applying fmap over a list - the function is applied to every element of the list, but the list itself is not altered.

-- Functor is a typeclass for function application over some structure f that we want to ignore and leave untouched.

-- For lists, map and fmap are identical but fmap can apply a function over any of the functorial structures.

-- f is called a type constructor because it's a higher-kinded type (it must be * -> *)
-- A type constant is a type with kind *


-----------------------------
-- Intermission: Exercises
-----------------------------

-- Given a type signature, determine the kinds of each type variable:
-- 1. What’s the kind of 𝑎?
-- a -> a
-- a :: *

-- 2. What are the kinds of 𝑏 and 𝑇? (The 𝑇 is capitalized on purpose!)
-- a -> b a -> T (b a)
-- b :: * -> *
-- T :: * -> *

-- 3. What’s the kind of 𝑐?
-- c a b -> c b a
-- c :: * -> * -> *


data FixMePls a =
    FixMe
    | Pls a
    deriving (Eq,Show)

instance Functor FixMePls where
    fmap _ FixMe   = FixMe
    fmap f (Pls a) = Pls (f a)
