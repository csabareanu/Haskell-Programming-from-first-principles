module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties we just showed you to validate them.

-- QuickCheck props
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
-- !!! Here f is not a function
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) =>
                    f a
                    -> Fun a b
                    -> Fun b c
                    -> Bool

functorCompose x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)


type IntToInt = Fun Int Int
type IntToString = Fun Int String
type StringToInt = Fun String Int


-- 1.
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IdentInt = Identity Int
type CompInt = IdentInt -> IntToString -> StringToInt -> Bool


-- 2.
data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f a)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a

type PairIdent = Pair Int
type PairComp = PairIdent -> IntToString -> StringToInt -> Bool


-- 3.
data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoIdent = Two Int Int
type TwoComp = TwoIdent -> IntToString -> StringToInt -> Bool



-- 4.
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeIdent = Three Int Int Int
type ThreeComp = ThreeIdent -> IntToString -> StringToInt -> Bool

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


main = do
    putStrLn "Exercise 1: Identity"
    quickCheck (functorIdentity :: IdentInt -> Bool)
    quickCheck (functorCompose :: CompInt)
    putStrLn ""

    putStrLn "Exercise 2: Pair"
    quickCheck (functorIdentity :: PairIdent -> Bool)
    quickCheck (functorCompose :: PairComp)
    putStrLn ""

    putStrLn "Exercise 3: Two"
    quickCheck (functorIdentity :: TwoIdent -> Bool)
    quickCheck (functorCompose :: TwoComp)
    putStrLn ""

    putStrLn "Exercise 4: Three"
    quickCheck (functorIdentity :: ThreeIdent -> Bool)
    quickCheck (functorCompose :: ThreeComp)
    putStrLn ""
