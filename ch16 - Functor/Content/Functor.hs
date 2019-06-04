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


--------------------
-- FUNCTOR LAWS
--------------------
-- Must abide 2 laws:
-- 1) IDENTITY : fmap id == id
-- 2) COMPOSITION

-- fmap (f . g) = fmap f . fmap g
-- Prelude> fmap ((+1) . (*5)) [1..5]
-- [6,11,16,21,26]
-- Prelude> fmap (+1) . fmap (*5) $[1..5]
-- [6,11,16,21,26]


data WhoCares a =
    ItDoesnt             -- only structure
    | Matter a           -- value we can fmap over inside the structure
    | WhatThisIsCalled   -- only structure
    deriving (Eq, Show)

-- law abiding
instance Functor WhoCares where
    fmap _ ItDoesnt         = ItDoesnt
    fmap f (Matter a)       = Matter (f a)
    fmap _ WhatThisIsCalled = WhatThisIsCalled

--------------------------
-- Identity law breaking
--------------------------

-- instance Functor WhoCares where
--     fmap _ ItDoesnt         = WhatThisIsCalled
--     fmap f WhatThisIsCalled = ItDoesnt
--     fmap f (Matter a)       = Matter (f a)

-- Prelude> fmap id ItDoesnt
-- WhatThisIsCalled
-- Prelude> fmap id WhatThisIsCalled
-- ItDoesnt
-- Prelude> fmap id ItDoesnt == id ItDoesnt
-- False
-- Prelude> fmap id WhatThisIsCalled == id WhatThisIsCalled
-- False

-------------------------------
-- Composability law breaking
-------------------------------

data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

-- NOT OKAY
-- instance Functor CountingBad where
--     fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
--     (a->b)       f       a  =      f               b

-- Prelude> let oneWhoKnocks = Heisenberg 0 "Uncle"
-- Prelude> fmap (++" Jesse") oneWhoKnocks
-- Heisenberg 1 "Uncle Jesse"
-- Prelude> fmap ((++" Jesse") . (++" lol")) oneWhoKnocks
-- Heisenberg 1 "Uncle lol Jesse"

-- Prelude> let f = (++" Jesse")
-- Prelude> let g = (++" lol")
-- Prelude> fmap (f . g) oneWhoKnocks
-- Heisenberg 1 "Uncle lol Jesse"
-- Prelude> fmap f . fmap g $ oneWhoKnocks
-- Heisenberg 2 "Uncle lol Jesse"

-- OKAY
instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n) (f a)



----------------------------------------------------------------------------------------------
-- What happens to the outer most type arguments when we can only transform the innermost ?
----------------------------------------------------------------------------------------------

-- (,)
data Two a b =
    Two a b
    deriving (Eq, Show)

-- Either
data Or a b =
    First a
    | Second b
    deriving (Eq, Show)

-- Both of the above have kind * -> * -> * which is not compatible with Functor (Functor must have kind * -> *)
-- By partially applying some arguments, we can reduce the kindness of the type.
-- For these given types, it suffices to apply one of the arguments of each type constructor, giving us kind * -> *

-- Prelude> :k Either
-- Either :: * -> * -> *
-- Prelude> :k Either Int
-- Either Int :: * -> *
-- Prelude> :k Either Int String
-- Either Int String :: *

---------------
-- WON'T WORK
---------------
-- instance Functor Two where
--     fmap = undefined

-- instance Functor Or where
--     fmap = undefined


-------------------
-- WILL WORK
-------------------
instance Functor (Two a) where
    -- WON'T WORK because a is now part of the functorial structure, the f.
    -- fmap f (Two a b) = Two $ (f a) (f b)
    -- WILL WORK
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)
