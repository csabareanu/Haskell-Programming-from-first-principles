module FunctorInstances where

import Test.QuickCheck

-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties we just showed you to validate them.

-- QuickCheck props
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
-- !!! Here f is not a function
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
    (fmap (g . f) x) == (fmap g . fmap f $ x)


-- 1.
newtype Identity a = Identity a


-- 2.
data Pair a = Pair a a


-- 3.
data Two a b = Two a b


-- 4.
data Three a b c = Three a b c


-- 5.
data Three' a b = Three' a b b


-- 6.
data Four a b c d = Four a b c d


-- 7.
data Four' a b = Four' a a a b


-- 8. Can you implement one for this type? Why? Why not?
data Trivial = Trivial


-- Doing these exercises is critical to understanding how Functors work,
-- do not skip past them!