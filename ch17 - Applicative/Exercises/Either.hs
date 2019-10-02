module Either where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
-- Write the Either Applicative that short-circuits on any error values.
-- Sum and Validation are both just alternative names for Either, but
-- you’ll be giving them different Applicative instances. See above for
-- an idea of how Validation should behave. Use the checkers library.

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

data Validation e a =
    Error e
    | Succ a
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First x)  _           = First x
    (<*>) _          (First y)   = First y
    (<*>) (Second f) (Second x)  = Second (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [
                     (1, return $ First a)
                    ,(1, return $ Second b)
                    ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

    -- same as Sum/Either
instance Functor (Validation e) where
    fmap _ (Error e)   = Error e
    fmap f (Succ e)    = Succ (f e)

    -- This is different
instance Monoid e => Applicative (Validation e) where
    pure                      = Succ
    (<*>) (Error a) (Error b) = Error (a <> b)
    (<*>) (Error a) _         = Error a
    (<*>) _         (Error b) = Error b
    (<*>) (Succ f)  (Succ b)  = Succ (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [
                     (1, return $ Error a)
                    ,(1, return $ Succ b)
                    ]

instance (Eq a, Eq b) => EqProp (Validation a b) where
    (=-=) = eq

-- Your hint for this one is that you’re writing the following functions:
--     applyIfBothSecond :: (Sum e) (a -> b)
--     -> (Sum e) a
--     -> (Sum e) b
--     applyMappendError :: Monoid e =>
--     (Validation e) (a -> b)
--     -> (Validation e) a
--     -> (Validation e) b
trigger :: Sum String (String, String, String)
trigger = undefined

trigger' :: Validation String (String, String, String)
trigger' = undefined

main :: IO ()
main = do
    putStrLn "Testing if Sum a b is a correct Applicative Instance"
    quickBatch $ applicative trigger
    putStrLn "Testing if Validation a b is a correct Applicative Instance"
    quickBatch $ applicative trigger'
