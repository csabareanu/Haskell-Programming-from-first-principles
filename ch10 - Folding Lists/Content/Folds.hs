module Folds where

-- Folds are catamorphisms (cata - down, against).
-- Catamorphisms are a means of deconstructing data.
-- Prelude> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- While map applies a function to each element of a list and returns a list, foldr replaces the cons constructor with the function provided and reduces the list.

sum_rec :: [Integer] -> Integer
sum_rec [] = 0
sum_rec (x:xs) = x + sum_rec xs

len_rec :: [a] -> Integer
len_rec [] = 0
len_rec (_:xs) = 1 + len_rec xs

product_rec :: [Integer] -> Integer
product_rec [] = 1
product_rec (x:xs) = x * product_rec xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f acc [] = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs

-- The relationship between foldr and foldl is such that:
-- foldr f z xs = foldl (flip f) z (reverse xs)


-- The similarities between this and the recursive patterns we saw above
-- should be clear. The “rest of the fold,” (foldr f acc xs) is an argu-
-- ment to the function f we’re folding with. The acc is the accumulator,
-- sometimes called “zero,” of our fold. It provides a fallback value for
-- the empty list case and a second argument to begin our fold with. The
-- accumulator is often the identity for whatever function we’re folding
-- with, such as 0 for (+) and 1 for (*) .

-- foldl begins its reduction process by adding the acc value to the head of the list,
-- while foldr has added it to the final element of the list first

-- we can use scans to see how a fold evaluates (scanr, scanl)
-- Prelude> foldr (+) 0 [1..5]
-- 15
-- Prelude> scanr (+) 0 [1..5]
-- [15,14,12,9,5,0]

-- if you want to follow along,
-- use paper and not the REPL.
-- foldr (^) 2 [1..3]
-- (1 ^ (2 ^ (3 ^ 2)))
-- (1 ^ (2 ^ 9))
-- 1 ^ 512
-- 1
-- Contrast that with this:
-- foldl (^) 2 [1..3]
-- ((2 ^ 1) ^ 2) ^ 3
-- (2 ^ 2) ^ 3
-- 4 ^ 3
-- 64
