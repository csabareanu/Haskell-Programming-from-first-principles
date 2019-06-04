module QuickCheckFunctors where

import Test.QuickCheck

-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)


-- QuickCheck props
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
-- !!! Here f is not a function
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- *QuickCheckFunctors> quickCheck $ \x -> functorIdentity (x :: [Int])
-- +++ OK, passed 100 tests.
-- *QuickCheckFunctors> let li x = functorCompose (+1) (*2) (x :: [Int])
-- *QuickCheckFunctors> :t li
-- li :: [Int] -> Bool
-- *QuickCheckFunctors> quickCheck li
-- +++ OK, passed 100 tests.
