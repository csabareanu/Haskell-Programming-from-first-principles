module Chapter_ex where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
-- 1.
newtype Identity a =
    Identity a
    deriving (Eq,Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure                            = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

-- *Chapter_ex> sample (arbitrary :: Gen (Identity Int))
instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq



-- 2.
data Pair a =
    Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
    pure f                          = Pair f f
    (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

-- 3. This should look familiar.
data Two a b =
    Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure                      = Two mempty
    (<*>) (Two a f) (Two b c) = Two (a <> b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq



-- 4. data Three a b c = Three a b c



-- 5. data Three' a b = Three' a b b



-- 6. data Four a b c d = Four a b c d



-- 7. data Four' a b = Four' a a a b


main :: IO ()
main = do
    putStrLn "\nTesting 1. Identity"
    quickBatch $ applicative $ Identity ("a","b","c")
    putStrLn "\nTesting 2. Pair"
    quickBatch $ applicative $ Pair ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 3. Two"
    quickBatch $ applicative $ Two ("a","b","c") ("a","b","c")


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
