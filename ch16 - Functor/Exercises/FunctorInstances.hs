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
    fmap f (Pair a b) = Pair (f a) (f b)

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
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Three' a b b

type Three'Ident = Three' Int Int
type Three'Comp = Three'Ident -> IntToString -> StringToInt -> Bool


-- 6.
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourIdent = Four Int Int Int Int
type FourComp = FourIdent -> IntToString -> StringToInt -> Bool


-- 7.
data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Four' a a a b

type Four'Ident = Four' Int Int
type Four'Comp = Four'Ident -> IntToString -> StringToInt -> Bool


-- 8. Can you implement one for this type? Why? Why not?
data Trivial = Trivial
-- We cannot implement a Functor instance for this type because it has kind *.
-- A type must have at least kind * -> * to have a Functor Instance


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

    putStrLn "Exercise 5: Three'"
    quickCheck (functorIdentity :: Three'Ident -> Bool)
    quickCheck (functorCompose :: Three'Comp)
    putStrLn ""

    putStrLn "Exercise 6: Four"
    quickCheck (functorIdentity :: FourIdent -> Bool)
    quickCheck (functorCompose :: FourComp)
    putStrLn ""

    putStrLn "Exercise 7: Four'"
    quickCheck (functorIdentity :: Four'Ident -> Bool)
    quickCheck (functorCompose :: Four'Comp)
    putStrLn ""
