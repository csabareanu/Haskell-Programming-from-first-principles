module Chapter_ex where

import Control.Applicative (liftA3)

-- Given a type that has an instance of Applicative, specialize the types
-- of the methods. Test your specialization in the REPL.

-- 1. -- Type
-- []
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]


-- 2. -- Type
-- IO
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b


-- 3. -- Type
-- (,) a
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> (t, a)
-- (<*>) :: (t, (a -> b)) -> (t, a) -> (t, b)



-- 4. -- Type
-- (->) e
-- -- Methods
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> (t -> a)
-- (<*>) :: (t -> (a -> b)) -> (t -> a) -> (t -> b)


-- Write applicative instances for the following datatypes. Confused?
-- Write out what the type should be. Use the checkers library to validate
-- the instances.
-- 1. newtype Identity a = Identity a deriving Show



-- 2. data Pair a = Pair a a deriving Show



-- 3. This should look familiar.
-- data Two a b = Two a b



-- 4. data Three a b c = Three a b c



-- 5. data Three' a b = Three' a b b



-- 6. data Four a b c d = Four a b c d



-- 7. data Four' a b = Four' a a a b


------------------
-- Combinations
------------------

-- Remember the vowels and stops exercise in folds? Reimplement the
-- combos function using liftA3 from Control.Applicative.

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = undefined
