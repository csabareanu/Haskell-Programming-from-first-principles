module Defining where

-- Applicative is a Monoidal Functor


-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b -- tie fighter or ap (short for apply)

-- The pure function embeds something into functorial structure

-- Prelude> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Prelude> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b


-- fmap f x = pure f <*> x
-- Prelude> fmap (+1) [1,2,3]
-- [2,3,4]
-- Prelude> pure (+1) <*> [1..3]
-- [2,3,4]

-- Prelude> pure 1 :: [Int]
-- [1]
-- Prelude> pure 1 :: Maybe Int
-- Just 1
-- Prelude> pure 1 :: Either a Int
-- Right 1
-- Prelude> pure 1 :: ([a], Int)
-- ([],1)


-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b


-- With Applicative, we have a Monoid for our structure and function
-- application for our values!

-- mappend :: f             f      f
-- $       ::   (a -> b)      a      b
-- (<*>)   :: f (a -> b) -> f a -> f b


-- -- List
-- [(*2), (*3)] <*> [4, 5]

-- =
-- [2 * 4, 2 * 5, 3 * 4, 3 * 5]

-- -- reduced
-- [8,10,12,15]



-- So what was (a -> b) enriched with in f (a -> b) -> f a -> f
-- b? In this case, “list-ness”. Although the actual application of each
-- (a -> b) to a value of type 𝑎 is quite ordinary, we now have a list of
-- functions rather than a single function as would be the case if it was
-- just the List Functor.

-- Prelude> Just (*2) <*> Just 2
-- Just 4
-- Prelude> Just (*2) <*> Nothing
-- Nothing
-- Prelude> Nothing <*> Just 2
-- Nothing
-- Prelude> Nothing <*> Nothing
-- Nothing