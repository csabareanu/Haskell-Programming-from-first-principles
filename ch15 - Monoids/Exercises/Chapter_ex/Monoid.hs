module Monoid where

import Test.QuickCheck
import Test.Hspec
import Data.Monoid


------------------------
-- MONOID EXERCISES
------------------------

-- Given a datatype, implement the Monoid instance. Add Monoid
-- constraints to type variables where needed. For the datatypes you’ve
-- already implemented Semigroup instances for, you just need to figure
-- out what the identity value is.

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a `mappend` mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mempty `mappend` a == a

-- 1. Again, validate all of your instances with QuickCheck. Example
-- scaffold is provided for the Trivial type.

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
    arbitrary = return Trivial


-- 2.
newtype Identity a =
    Identity a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

type IdentityTest = Identity [Int]

type IdentityAssoc = IdentityTest -> IdentityTest -> IdentityTest -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a


-- 3.
data Two a b =
    Two a b
    deriving (Eq,Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoTest = Two [Int] String

type TwoAssoc = TwoTest -> TwoTest -> TwoTest -> Bool


-- 4.
newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)
-- What it should do:
-- Prelude> (BoolConj True) `mappend` mempty
-- BoolConj True
-- Prelude> mempty `mappend` (BoolConj False)
-- BoolConj False

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _               <> _               = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
    arbitrary = frequency [(1, return $ BoolConj True),
                            (1, return $ BoolConj False)]


-- 5.
newtype BoolDisj =
    BoolDisj Bool
    deriving (Eq, Show)

-- What it should do:
-- Prelude> (BoolDisj True) `mappend` mempty
-- BoolDisj True
-- Prelude> mempty `mappend` (BoolDisj False)
-- BoolDisj False

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _               <>  _                = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
    arbitrary = frequency [(1, return $ BoolDisj True),
                            (1, return $ BoolDisj False)]


-- 6.
newtype Combine a b =
    Combine { unCombine :: (a -> b) }

-- What it should do:
-- Prelude> let f = Combine $ \n -> Sum (n + 1)
-- Prelude> unCombine (mappend f mempty) $ 1
-- Sum {getSum = 2}

instance Semigroup b => Semigroup (Combine a b) where
    f <> g = Combine (unCombine f <> unCombine g)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\ _ -> mempty)

-- 7. Hint: We can do something that seems a little more specific and
-- natural to functions now that the input and output types are the
-- same.

-- newtype Comp a =
--     Comp (a -> a)
-- or
newtype Comp a =
    Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
    Comp f <> Comp g = Comp (\x -> (f x) <> (g x))

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
    mempty = Comp (\ _ -> mempty)


-- 8. This next exercise will involve doing something that will feel
-- a bit unnatural still and you may find it difficult. If you get it
-- and you haven’t done much FP or Haskell before, get yourself a
-- nice beverage. We’re going to toss you the instance declaration
-- so you don’t churn on a missing Monoid constraint you didn’t
-- know you needed.
newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

combineMems f g x =
    let (a,b) = f x
        (c,d) = g b
    in
        (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
    f <> g = Mem $ combineMems (runMem f) (runMem g)

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty = Mem (\x -> (mempty, x))

-- instance Monoid a => Monoid (Mem s a) where
-- mempty = undefined
-- mappend = undefined
-- Given the following code:
f' = Mem $ \s -> ("hi", s + 1)
-- main = do
-- print $ runMem (f' <> mempty) 0
-- print $ runMem (mempty <> f') 0
-- print $ (runMem mempty 0 :: (String, Int))
-- print $ runMem (f' <> mempty) 0 == runMem f' 0
-- print $ runMem (mempty <> f') 0 == runMem f' 0
-- A correct Monoid for Mem should, given the above code, get the
-- following output:
-- Prelude> main
-- ("hi",1)
-- ("hi",1)
-- ("",0)
-- True
-- True
-- Make certain your instance has output like the above, this is
-- sanity-checking the Monoid identity laws for you! It’s not a
-- proof and it’s not even as good as quick-checking, but it’ll catch
-- the most common mistakes people make. If you’d like to learn
-- how to generate functions with QuickCheck, not just values, look
-- at CoArbitrary in QuickCheck’s documentation.
-- It’s not a trick and you don’t need a Monoid for 𝑠. Yes, such a
-- Monoid can and does exist. Hint: chain the s values from one
-- function to the other. You’ll want to validate your instance as
-- well, to do that, in particular you’ll want to check the identity laws
-- as a common mistake with this exercise is to write an instance
-- that doesn’t respect them.


main :: IO ()
main = do
    --1
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    --2
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: IdentityTest -> Bool)
    quickCheck (monoidRightIdentity :: IdentityTest -> Bool)
    --3
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: TwoTest -> Bool)
    quickCheck (monoidRightIdentity :: TwoTest -> Bool)
    --4
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    --5
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

    hspec $ do
        describe "Mem Testing" $ do
            it "runMem (f' <> mempty) 0 is ('hi',1)" $ do
                runMem (f' <> mempty) 0 `shouldBe` ("hi",1)
            it "runMem (mempty <> f') 0 is ('hi',1)" $ do
                runMem (mempty <> f') 0 `shouldBe` ("hi",1)
            it "(runMem mempty 0 :: (String, Int)) is ('',0)" $ do
                (runMem mempty 0 :: (String, Int)) `shouldBe` ("",0)
            it "runMem (f' <> mempty) 0 == runMem f' 0 is True" $ do
                runMem (f' <> mempty) 0 == runMem f' 0 `shouldBe` True
            it "runMem (mempty <> f') 0 == runMem f' 0 is True" $ do
                runMem (mempty <> f') 0 == runMem f' 0 `shouldBe` True