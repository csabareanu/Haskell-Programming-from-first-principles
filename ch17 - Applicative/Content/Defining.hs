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