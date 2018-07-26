module Scan where

-- scanl :: (a -> b -> a) -> a -> [b] -> [a]
-- scanl f q ls =
--     q : (case ls of
--             []   -> []
--             x:xs -> scanl f (f q x) xs)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- the element with index x
fibsN :: Int -> Integer
fibsN x = fibs !! x

-- all elements up to the one with index x
fibsList :: Int -> [Integer]
fibsList x = take x fibs

-- all elements in the Fibonacci seq smaller than x
fibsWhile :: Integer -> [Integer]
fibsWhile x = takeWhile (<= x) fibs


---------------
-- Factorial
---------------

fct :: [Integer]
fct = 1 : scanl (\y z -> (y + z) * (y `div` z))  1 fct

fctList :: Int -> [Integer]
fctList x = take (x + 1) fct

nfct :: Int -> Integer
nfct x = fct !! x

-- Solution reasoning:
-- We need access to the last 2 computed values:
-- [..., y, z, x!]
-- [..., (x - 2)! , (x - 1)!, x!]
-- [..., (x - 2)! , ((x - 2)! * (x - 1)) , x!]
-- Trying to add y + z
-- (x - 2)! + (x - 2)! * (x - 1)
-- (x - 2)! * (1 + (x - 1))
-- (x - 2)! * x =
-- (x - 1) is missing to have x!
-- Trying to divide z / y
-- (x - 2)! * (x - 1) / (x - 2)!
-- (x - 1)
-- => (\z y -> (y + z) * (z `div` y))