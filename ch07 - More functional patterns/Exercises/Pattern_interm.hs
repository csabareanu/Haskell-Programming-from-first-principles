module Pattern_interm where

-- 1. Given the following declarations
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- a) What is the type of k?
-- k :: (a, b) -> a

-- b) What is the type of k2? Is it the same type as k1 or k3?
-- k2 :: [Char]
-- k1 :: Integer
-- k3 :: Integer

-- c) Of k1, k2, k3, which will return the number 3 as the result?
-- k1 and k3



-- 2. Fill in the definition of the following function:
-- -- Remember: Tuples have the same syntax for their
-- -- type constructors and their data constructors.
-- f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
-- f = undefined
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, _, z) (t, _, v) = ((x, t), (z, v))