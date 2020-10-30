module Numeric where

-- Integral Numbers: whole numbers, positive and negative
-- 1) Int - it has a range, with a minimum and maximum. Is an artefact and not to be used if the limitations are not fully understood. (Int8, Int16, Int32, Int64 - GHC.Int)
-- 2) Integer - does not have a range

-- Fractional Numbers
-- 1) Float -- single precision floating point numbers. Use only when doing graphics programming such as with OpenGL.
-- 2) Double -- double precision floating point numbers
-- 3) Rational -- fractional number that represent the ratio of 2 Integers (1/2 :: Rational)
-- Prelude> :t (/)
-- (/) :: Fractional a => a -> a -> a
-- Fractional is a typeclass that requires types to already have an instance
-- of the Num typeclass. We describe this relationship between typeclasses
-- by saying that Num is a superclass of Fractional. So (+) and
-- other functions from the Num typeclass can be used with Fractional
-- numbers, but functions from the Fractional typeclass cannot be used
-- with all instances of Num

-- 4) Scientific -- stores the coeficient as an Integer and the exponent as Int

-- These numeric datatypes all have instances of typeclass Num
-- Definition. In Haskell, a TYPECLASS is a way of adding functionality to types that is reusable across all the types that have instances of that typeclass.
-- The Num typeclass is common for all types of numbers because it provides standard operators (+), (-), (*) and others .
-- Any type that has an instance of Num can be used with those functions.

-- For finding the maxBound or minBound of some numeric types (that have an instance of typeclass Bounded) use theese functions

-----------------------
-- Comparing values
-----------------------
-- /= -> is not equal to
-- Eq is a typeclass that includes everything that can be compared and determined to be equal in value.
-- Ord is a typeclass that includes everything that can be ordered. A datatype that has no instance of Ord will not work

-- In Haskell there are 6 categories of entities that have names:
-- 1) Variables (term level)
-- 2) Data Constructors (term level)
-- 3) Type Variables (type level)
-- 4) Type Constructors (type level)
-- 5) Typeclasses (type level)
-- 6) Modules



