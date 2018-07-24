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

-- The similarities between this and the recursive patterns we saw above
-- should be clear. The “rest of the fold,” (foldr f acc xs) is an argu-
-- ment to the function f we’re folding with. The acc is the accumulator,
-- sometimes called “zero,” of our fold. It provides a fallback value for
-- the empty list case and a second argument to begin our fold with. The
-- accumulator is often the identity for whatever function we’re folding
-- with, such as 0 for (+) and 1 for (*) .