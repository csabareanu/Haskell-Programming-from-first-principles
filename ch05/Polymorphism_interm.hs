module Polymorphism_interm where

-- 1. Given the type a -> a, which is the type for id, attempt to make
-- a function that is not bottom and terminates successfully that
-- does something other than returning the same value. This is
-- impossible, but you should try it anyway.

-- It is not possible, given the type a->a to have a function that does something than returning the same value.

-- 2. We can get a more comfortable appreciation of parametricity
-- by looking at a -> a -> a. This hypothetical function a -> a
-- -> a has two–and only two–implementations. Write both possible
-- versions of a -> a -> a. After doing so, try to violate the
-- constraints of parametrically polymorphic values we outlined
-- above.
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

-- 3. Implement a -> b -> b. How many implementations can it
-- have? Does the behavior change when the types of 𝑎 and 𝑏
-- change?

f3 :: x y = y
-- The behaviour should not change.