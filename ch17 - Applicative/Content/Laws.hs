module Laws where

-- 1. IDENTITY
-- pure id <*> v = v

--------------
-- EXAMPLES
--------------
-- Prelude> pure id <*> [1..5]
-- [1,2,3,4,5]
-- Prelude> [id] <*> [1..5]
-- [1,2,3,4,5]
-- Prelude> pure id <*> Just "Hello Applicative"
-- Just "Hello Applicative"
-- Prelude> pure id <*> Nothing
-- Nothing
-- Prelude> pure id <*> Right 8001
-- Right 8001

-- pure embeds our function into some structure so we can use apply instead of fmap


-- 2. COMPOSITION
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- The result of composing our functions first and then applying them is the same as the result of applying the functions first and then composing them

--------------
-- EXAMPLES
--------------
-- Prelude> pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3]
-- [3,5,7]
-- Prelude> [(+1)] <*> ([(*2)] <*> [1,2,3])
-- [3,5,7]
-- Prelude> pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1
-- Just 3
-- Prelude> Just (+1) <*> (Just (*2) <*> Just 1)
-- Just 3


-- 3. HOMOMORPHISM
-- A homomorphism is a structure-preserving map between 2 categories
-- The effect of applying a function that is embeded in some structure to a value that is embeded in the same structure should be
-- the same as applying a function to a value without affecting the outside structure.

-- pure f <*> pure x = pure (f x)
-- Prelude> pure (+1) <*> pure 1
-- 2
-- Prelude> pure ((+1) 1)
-- 2

-- the structure that pure provides in the above example is not really meaningful so the result should be the same with (+1) 1.
-- Prelude> pure (+1) <*> pure 1 :: [Int]
-- [2]
-- Prelude> pure ((+1) 1) :: [Int]
-- [2]


-- 4. INTERCHANGE
-- u <*> pure y = pure ($ y) <*> u -- u function embeded in some structure

-- Prelude> Just (+2) <*> pure 2
-- Just 4
-- Prelude> pure ($ 2) <*> Just (+2)
-- Just 4

-- Prelude> [(+1),(*2)] <*> pure 1
-- [2,2]
-- Prelude> pure ($ 1) <*> [(+1),(*2)]
-- [2,2]