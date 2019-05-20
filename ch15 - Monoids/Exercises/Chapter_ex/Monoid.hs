module Monoid where

import Test.QuickCheck


------------------------
-- MONOID EXERCISES
------------------------

-- Given a datatype, implement the Monoid instance. Add Monoid
-- constraints to type variables where needed. For the datatypes you’ve
-- already implemented Semigroup instances for, you just need to figure
-- out what the identity value is.

-- 1. Again, validate all of your instances with QuickCheck. Example
-- scaffold is provided for the Trivial type.

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    --mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
    arbitrary = return Trivial


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a `mappend` mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mempty `mappend` a == a

-- 2. newtype Identity a = Identity a deriving Show
-- 3. data Two a b = Two a b deriving Show
-- 4. newtype BoolConj =
-- BoolConj Bool
-- What it should do:
-- Prelude> (BoolConj True) `mappend` mempty
-- BoolConj True
-- Prelude> mempty `mappend` (BoolConj False)
-- BoolConj False
-- 5. newtype BoolDisj =
-- BoolDisj Bool
-- What it should do:
-- Prelude> (BoolDisj True) `mappend` mempty
-- BoolDisj True
-- Prelude> mempty `mappend` (BoolDisj False)
-- BoolDisj False
-- 6. newtype Combine a b =
-- Combine { unCombine :: (a -> b) }
-- What it should do:
-- Prelude> let f = Combine $ \n -> Sum (n + 1)
-- Prelude> unCombine (mappend f mempty) $ 1
-- Sum {getSum = 2}
-- 7. Hint: We can do something that seems a little more specific and
-- natural to functions now that the input and output types are the
-- same.
-- newtype Comp a =
-- Comp (a -> a)
-- 8. This next exercise will involve doing something that will feel
-- a bit unnatural still and you may find it difficult. If you get it
-- and you haven’t done much FP or Haskell before, get yourself a
-- nice beverage. We’re going to toss you the instance declaration
-- so you don’t churn on a missing Monoid constraint you didn’t
-- know you needed.
-- newtype Mem s a =
-- Mem {
-- runMem :: s -> (a,s)
-- }
-- instance Monoid a => Monoid (Mem s a) where
-- mempty = undefined
-- mappend = undefined
-- Given the following code:
-- f' = Mem $ \s -> ("hi", s + 1)
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