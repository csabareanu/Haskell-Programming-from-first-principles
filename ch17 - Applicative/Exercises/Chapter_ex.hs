module Chapter_ex where

import Control.Applicative (liftA3)
import Data.Monoid
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

-- testing


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



-- 4.
data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure                              = Three mempty mempty
    (<*>) (Three a b f) (Three c d e) = Three (a <> c) (b <> d) (f e)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq



-- 5. data Three' a b = Three' a b b
data Three' a b =
    Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
    pure a                              = Three' mempty a a
    (<*>) (Three' a f g) (Three' b c d) = Three' (a <> b) (f c) (g d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq



-- 6. data Four a b c d = Four a b c d

data Four a b c d =
    Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure                                = Four mempty mempty mempty
    (<*>) (Four a b c f) (Four d e i j) = Four (a <> d) (b <> e) (c <> i) (f j)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq



-- 7. data Four' a b = Four' a a a b
data Four' a b =
    Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
    pure                                  = Four' mempty mempty mempty
    (<*>) (Four' a b c f) (Four' d e i j) = Four' (a <> d) (b <> e) (c <> i) (f j)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

main :: IO ()
main = do
    putStrLn "\nTesting 1. Identity"
    quickBatch $ applicative $ Identity ("a","b","c")
    putStrLn "\nTesting 2. Pair"
    quickBatch $ applicative $ Pair ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 3. Two"
    quickBatch $ applicative $ Two ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 4. Three"
    quickBatch $ applicative $ Three ("a","b","c") ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 5. Three'"
    quickBatch $ applicative $ Three' ("a","b","c") ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 6. Four"
    quickBatch $ applicative $ Four ("a","b","c") ("a","b","c") ("a","b","c") ("a","b","c")
    putStrLn "\nTesting 7. Four'"
    quickBatch $ applicative $ Four' ("a","b","c") ("a","b","c") ("a","b","c") ("a","b","c")


------------------
-- Combinations
------------------

-- Remember the vowels and stops exercise in folds? Reimplement the
-- combos function using liftA3 from Control.Applicative.

-- Write a function that takes inputs from stops and vowels
-- and makes 3-tuples of all possible stop-vowel-stop combina-
-- tions. These will not all correspond to real words in English,
-- although the stop-vowel-stop pattern is common enough
-- that many of them will.


stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos = undefined
combos l1 l2 l3 = liftA3 (,,) l1 l2 l3
