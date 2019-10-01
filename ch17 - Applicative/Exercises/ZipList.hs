module ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Implement the ZipList Applicative. Use the checkers library to validate
-- your Applicative instance. We’re going to provide the EqProp instance
-- and explain the weirdness in a moment.

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _          = Nil
take' _ Nil        = Nil
take' n (Cons h t)
    | n == 0    = Nil
    | otherwise = Cons h (take' (n - 1) t)

instance Functor List where
    fmap _ Nil              = Nil
    fmap f (Cons head tail) = Cons (f head) (fmap f tail)

-- In ListApplicative.hs
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f
--------------------------------

instance Applicative List where
    pure f                = Cons f Nil
    (<*>) _           Nil = Nil
    (<*>) Nil         _   = Nil
    (<*>) fs          xs  = flatMap (`fmap` xs) fs

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' =
                let (ZipList' l) = xs
                    in take' 1000 l
              ys' =
                let (ZipList' l) = ys
                    in take' 1000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs


instance Applicative ZipList' where
    pure f = ZipList' (Cons f Nil)
    (<*>) _                     (ZipList' Nil)                 = ZipList' Nil
    (<*>) (ZipList' Nil)         _                             = ZipList' Nil
    (<*>) (ZipList' fs)         (ZipList' xs)                  = ZipList' $ zipWith' fs xs
        where   zipWith' (Cons f Nil) (Cons x xs)  = Cons (f x) (pure f <*> xs)
                zipWith' (Cons f fs)  (Cons x Nil) = Cons (f x) (fs <*> pure x)
                zipWith' (Cons f fs)  (Cons x xs)  = Cons (f x) (zipWith' fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary


instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        as <- arbitrary
        elements [Nil, Cons a as]

main :: IO ()
main = quickBatch (applicative (ZipList' (Cons ("a", "b", "b") Nil)))


-- A couple hints: think infinitely. Check Prelude for functions that can
-- give you what you need. One starts with the letter z, the other with
-- the letter r. You’re looking for inspiration from these functions, not
-- to be able to directly reuse them as you’re using a custom List type
-- and not the provided Prelude list type.