module Fractional where


-- divideThenAdd :: Num a => a -> a -> a
-- divideThenAdd x y = (x / y) + 1

-- This won't work bc '/' is defined in the Fractional typeclass and Num does not have an instance of Fractional.
-- For that to work, the function signature should be
-- divideThenAdd :: Fractional a => a -> a -> a

-- Bc of type defaulting, for Fractional it means that (/) :: Fractional a => a -> a -> a se schimba in (/) :: Double -> Double -> Double daca nu se specifica type-ul

-- Because (+) and (-) are defined in Num typeclass this will work just fine:

substractThenAdd :: Num a => a -> a -> a
substractThenAdd x y = (x - y) + 1


-- Why didn’t we need to make the type of
-- the function we wrote require both typeclasses? Why didn’t we have
-- to do this:
-- f :: (Num a, Fractional a) => a -> a -> a

-- Fractional requires an instance of Num by definition so Num is redundant