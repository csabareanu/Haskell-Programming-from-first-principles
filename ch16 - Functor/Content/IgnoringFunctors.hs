module IgnoringFunctors where

-- the Functor instances of Maybe and Either are used when you want to ignore the left cases, used tipically as error or failure cases
-- Because fmap does not touch the left values, you can map the function right on the values you are working with and ignore the faulty cases

--------------
-- MAYBE -----
--------------

-- pattern matching with Maybe

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just (n + 1)
incIfJust Nothing  = Nothing

showIfJust :: (Show a) => Maybe a -> Maybe String
showIfJust (Just s) = Just (show s)
showIfJust Nothing  = Nothing

-- fmapping with Maybe. Is equivalent with the above.

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

showMaybe :: (Show a) => Maybe a -> Maybe String
showMaybe = fmap show

--------------------
-- Short Exercise
--------------------

-- Write a Functor instance for a datatype identical to Maybe. We’ll use
-- our own datatype because Maybe already has a Functor instance and
-- we cannot make a duplicate one.
data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- If it helps, you’re basically writing the following function:
-- applyIfJust :: (a -> b) -> Maybe a -> Maybe b


----------------
-- EITHER
----------------

-- It is used when you want to preserve the reason why a computation has failed rather than only the fact that it failed .

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: (Show a) => Either e a -> Either e String
showEither' = fmap show

-------------------
-- Short Exercise
-------------------
-- 1. Write a Functor instance for a datatype identical to Either. We’ll
-- use our own datatype because Either also already has a Functor
-- instance.
data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

-- Your hint for this one is that you’re writing the following function.
-- applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b

-- 2. Why is a Functor instance that applies the function only to First,
-- Either’s Left, impossible? We covered this earlier.

-- Because the datatype that has an instance of Functor must have Kind * -> * . To apply the fuction to First, we must have kind * -> * -> * which is impossible.
