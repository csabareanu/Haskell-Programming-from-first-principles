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
fct = 1 : scanl (\x y -> (x * (y + 2)))  2 fct
-- 1 : scanl (*) 2 (1 : scanl (*) 2 fct
-- 1 : 2 : (scanl (*) ((*) 2 1)  (1 : scanl (*) 2 fct))
-- 1 : 2 : 2 : (scanl (*) )

fctList :: Int -> [Integer]
fctList x = take x fct