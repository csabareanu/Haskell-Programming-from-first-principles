module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
      Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools)
                          ,(1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty      = Fools
    mappend _ _ = Fools

instance EqProp Bull where
    (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)

-- *BadMonoid> main

-- monoid:
--   left  identity: *** Failed! Falsified (after 2 tests):
-- Twoo
--   right identity: *** Failed! Falsified (after 2 tests):
-- Twoo
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.



-- *BadMonoid> quickBatch $ applicative [("b","w",1)]

-- applicative:
--   identity:     +++ OK, passed 500 tests.
--   composition:  +++ OK, passed 500 tests.
--   homomorphism: +++ OK, passed 500 tests.
--   interchange:  +++ OK, passed 500 tests.
--   functor:      +++ OK, passed 500 tests.

-- *BadMonoid> let trigger = undefined :: [(String, String, Int)]
-- *BadMonoid> quickBatch (applicative trigger)

-- applicative:
--   identity:     +++ OK, passed 500 tests.
--   composition:  +++ OK, passed 500 tests.
--   homomorphism: +++ OK, passed 500 tests.
--   interchange:  +++ OK, passed 500 tests.
--   functor:      +++ OK, passed 500 tests.
