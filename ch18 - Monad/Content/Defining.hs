-- Are applicative functors
-- http://blog.plover.com/prog/burritos.

-- A FUNCTOR maps a function over some structure
-- An APPLICATIVE maps a function that is contained over some structure over some structure and then mappends the two bits of structure
-- A MONAD is just another way of applying functions over structure with a couple of additional features.

-- class Applicative m => Monad (m :: * -> *) where
--     (>>=) :: m a -> (a -> m b) -> m b  -- MINIMAL (bind operator) - contains the things that are special about monads.
--     (>>) :: m a -> m b -> m b          -- (sequencing operator) - sequences 2 actions and discards the resulting value of the first one.
--     return :: a -> m a                 -- The same as pure. Takes a value and returns it inside a structure

-- You can write fmap using monadic operations because Applicative is a superclass of Monad and Functor is a superclass of Applicative
-- Functor -> Applicative -> Monad

-- Prelude> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Prelude> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- fmap f xs = xs >>= return . f
-- Prelude> fmap (+1) [1..3]
-- [2,3,4]
-- Prelude> [1..3] >>= return . (+1)
-- [2,3,4]


-- fmap :: Functor f     =>   (a -> b) -> f a        -> f b
-- <*>  :: Applicative f => f (a -> b) -> f a        -> f b
-- >>=  :: Monad f       => f a        -> (a -> f b) -> f b

-- If b == f b
-- fmap :: Functor f     =>   (a -> f b) -> f a        -> f (f b)

-- Prelude> let andOne x = [x,1]            -- a -> f b (a -> [] b)
-- Prelude> andOne 10
-- [10,1]
-- Prelude> :t fmap andOne [4,5,6]          -- f a ([] a)
-- fmap andOne [4,5,6] :: Num a => [[a]]
-- Prelude> fmap andOne [4,5,6]
-- [[4,1],[5,1],[6,1]]                      -- f (f b)  ([[]] b) - extra layer of structure.

-- Prelude> concat $ fmap andOne [1,2,3]    -- does not generate additional monadic structure
-- [1,1,2,1,3,1]

-- In a sense, Monad is a generalization of concat
-- The ability to flatten 2 layers of structure into one is what makes monad special.

-- So how do we get bind?
-- The answer is the exercise Write bind in terms of fmap and join.
-- Fear is the mind-killer, friend. You can do it.

-- import Control.Monad (join)
import Control.Monad

--join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs


-- Monad is not:
-- 1. Impure -- Monads are pure functions
-- 2. An embedded language for imperative programming.
-- 3. A value : The typeclass describes a specific relationship between elements in a domain and defines some operations over them.
-- 4. About strictness

-- THE MONAD typeclass is generalized structure manipulation with some laws to make it sensible (just like Functor or applicative)

-- The MONAD class also contains lift functions, similar with the ones in Applicative. The functions are used for historical reasons.
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r

-- Prelude> liftA2 (,) (Just 3) (Just 5)
-- Just (3,5)
-- Prelude> liftM2 (,) (Just 3) (Just 5)
-- Just (3,5)

-- liftA2 or liftM2 is zipWith specialized in lists
liftM2  :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
zipWith ::            (a -> b -> c)   -> [a]  -> [b]  -> [c]

-- *Main> zipWith (+) [1,2,3] [1,2,3]
-- [2,4,6]
