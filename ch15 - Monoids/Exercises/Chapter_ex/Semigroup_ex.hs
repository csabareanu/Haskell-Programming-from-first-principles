module Semigroup_ex where

import Test.QuickCheck
import Data.Monoid
import Data.Semigroup

-- Given a datatype, implement the Semigroup instance. Add Semigroup
-- constraints to type variables where needed. Use the Semigroup
-- class from the semigroups library or write your own. When we use
-- <>, we mean the infix mappend from the Semigroup typeclass.
-- Note We’re not always going to derive every instance you may want
-- or need in the datatypes we provide for exercises. We expect you to
-- know what you need and to take care of it yourself by this point.


-- 1. Validate all of your instances with QuickCheck. Since Semigroup’s
-- only law is associativity, that’s the only property you
-- need to reuse.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool --1


-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssocString = Identity String -> Identity String -> Identity String -> Bool
type IdentityAssocList = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool


-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two c d = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc = Three String [Int] Trivial -> Three String [Int] Trivial -> Three String [Int] Trivial -> Bool

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    Four a b c d <> Four e f g h = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourAssoc = Four Trivial (Identity String) String [String]
              -> Four Trivial (Identity String) String [String]
              -> Four Trivial (Identity String) String [String]
              -> Bool

-- 6.
newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)
-- What it should do:
-- Prelude> (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- Prelude> (BoolConj True) <> (BoolConj False)
-- BoolConj False

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _             <> _             = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = frequency [(1, return $ BoolConj True),
                           (1, return $ BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj =
    BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    _              <> _              = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = frequency [(1, return $ BoolDisj True),
                           (1, return $ BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- What it should do:
-- Prelude> (BoolDisj True) <> (BoolDisj True)
-- BoolDisj True
-- Prelude> (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True


-- 8.
data Or a b =
        Fst a
      | Snd b
      deriving (Eq, Show)
-- The Semigroup for Or should have the following behavior. We
-- can think of this as having a “sticky” Snd value where it’ll hold
-- onto the first Snd value when and if one is passed as an argument.
-- This is similar to the First’ Monoid you wrote earlier.
-- Prelude> Fst 1 <> Snd 2
-- Snd 2
-- Prelude> Fst 1 <> Fst 2
-- Fst 2
-- Prelude> Snd 1 <> Fst 2
-- Snd 1
-- Prelude> Snd 1 <> Snd 2
-- Snd 1

instance Semigroup (Or a b) where
    (Fst x) <> t@(_) = t
    (Snd x) <> _     = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [ return $ Fst a, return $ Snd b ]

type OrTypes = Or String Int

type OrAssoc = OrTypes -> OrTypes -> OrTypes -> Bool

-- 9.
newtype Combine a b =
    Combine { unCombine :: (a -> b) }

-- What it should do:
-- Prelude> let f = Combine $ \n -> Sum (n + 1)
-- Prelude> let g = Combine $ \n -> Sum (n - 1)
-- Prelude> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- Prelude> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- Prelude> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- Prelude> unCombine (g <> f) $ 1
-- Sum {getSum = 2}
-- Hint: This function will eventually be applied to a single value
-- of type 𝑎. But you’ll have multiple functions that can produce a
-- value of type 𝑏. How do we combine multiple values so we have
-- a single 𝑏? This one will probably be tricky! Remember that the
-- type of the value inside of Combine is that of a function. If you
-- can’t figure out CoArbitrary, don’t worry about QuickChecking
-- this one.

instance (Semigroup b) => Semigroup (Combine a b) where
    f <> g = Combine { unCombine = (unCombine f <> unCombine g) }

-- https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html

-- 10.
newtype Comp a =
    Comp { unComp :: (a -> a) }

-- Hint: We can do something that seems a little more specific and
-- natural to functions now that the input and output types are the
-- same.

instance (Semigroup a) => Semigroup (Comp a) where
    f <> g = Comp { unComp = (unComp f <> unComp g) }


-- 11. -- Look familiar?
data Validation a b =
    Failure a
    | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Semigroup_ex.Success x) <> _                        = Semigroup_ex.Success x
    (Semigroup_ex.Failure x) <> (Semigroup_ex.Success y) = Semigroup_ex.Success y
    (Semigroup_ex.Failure x) <> (Semigroup_ex.Failure y) = Semigroup_ex.Failure (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Semigroup_ex.Failure a,
               return $ Semigroup_ex.Success b
                ]

type ValidationType = Validation String Int

type ValidationAssoc = ValidationType -> ValidationType -> ValidationType -> Bool






-- 12. -- Validation with a Semigroup
-- -- that does something different
-- newtype AccumulateRight a b =
-- AccumulateRight (Validation a b)
-- deriving (Eq, Show)
-- instance Semigroup b =>
-- Semigroup (AccumulateRight a b) where
-- (<>) = undefined
-- 13. -- Validation with a Semigroup
-- -- that does something more
-- newtype AccumulateBoth a b =
-- AccumulateBoth (Validation a b)
-- deriving (Eq, Show)
-- instance (Semigroup a, Semigroup b) =>
-- Semigroup (AccumulateBoth a b) where
-- (<>) = undefined


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc) --1
    quickCheck (semigroupAssoc :: IdentityAssocString) --2
    quickCheck (semigroupAssoc :: IdentityAssocList) --2
    quickCheck (semigroupAssoc :: TwoAssoc) --3
    quickCheck (semigroupAssoc :: ThreeAssoc) --4
    quickCheck (semigroupAssoc :: FourAssoc) --5
    quickCheck (semigroupAssoc :: BoolConjAssoc) --6
    quickCheck (semigroupAssoc :: BoolDisjAssoc) --7
    quickCheck (semigroupAssoc :: OrAssoc) --8
    quickCheck (semigroupAssoc :: ValidationAssoc)