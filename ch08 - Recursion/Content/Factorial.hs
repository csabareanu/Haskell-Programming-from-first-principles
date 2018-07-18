module Factorial where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

-- -- This won't work. It never stops.
-- brokenFact1 :: Integer -> Integer
-- brokenFact1 n = n * brokenFact1 (n - 1)

-- -- To make my point obvious, let's apply this to 4
-- -- and see what happens
-- brokenFact1 4 = 4 * (4 - 1)
--                   * ((4 - 1) - 1)
--                   * (((4 - 1) - 1) - 1)
--                   ... this series never stops

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial_guards :: Integer -> Integer
factorial_guards x
    | x == 0 = 1
    | otherwise = x * factorial_guards (x - 1)


-- We can think recursion as function composition but where the first result gets passed back to the same function
-- rather than a different one , until it reaches the base case and terminates. If it has no base case the application
-- can be infinite.
-------------------------------------------------
-- Recursion is self-referential composition     |
-------------------------------------------------

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0     n = n
incTimes times n = 1 + (incTimes (times - 1) n)

-- abstract the recursion by defining a function that applies a function n times to a parameter b
applyTimes :: (Eq a, Num a) =>
                a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b -- or f $ applyTimes or parantheses

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n