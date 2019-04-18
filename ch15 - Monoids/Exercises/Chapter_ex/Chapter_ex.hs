module Chapter_ex where

import Data.Monoid
import Test.QuickCheck


-- Write the Monoid instance for our Maybe type renamed to Optional.
data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

--     instance Monoid a => Monoid (Optional a) where
--     mempty = undefined
--     mappend = undefined

instance Monoid a => Monoid (Optional a) where
    mempty                    = Nada
    mappend Nada     (Only x) = Only $ mappend mempty x
    mappend (Only x) Nada     = Only $ mappend x      mempty
    mappend (Only x) (Only y) = Only $ mappend x      y


-- Expected output:
-- Prelude> Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Sum {getSum = 2})
-- Prelude> Only (Product 4) `mappend` Only (Product 2)
-- Only (Product {getProduct = 8})
-- Prelude> Only (Sum 1) `mappend` Nada
-- Only (Sum {getSum = 1})
-- Prelude> Only [1] `mappend` Nada
-- Only [1]
-- Prelude> Nada `mappend` Only (Sum 1)
-- Only (Sum {getSum = 1})

----------------
--Exercise 2 ---
----------------

-- Write a Monoid instance for Maybe type which doesn’t require a
-- Monoid for the contents. Reuse the Monoid law QuickCheck properties
-- and use them to validate the instance.

type S = String
type B = Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = (mempty <> a) == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = (a <> mempty) == a

-- record syntax : page 400
newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance (Monoid a) => Monoid (First' a) where
    mempty                                          = First' { getFirst' = Nada }
    mappend (First' Nada)       (First' (Only x))   = First' { getFirst' = Only x }
    mappend (First' (Only x))   (First' Nada)       = First' { getFirst' = Only x }
    mappend (First' (Only x))   (First' (Only y))   = First' { getFirst' = Only $ mappend x y  }
    mappend _                   _                   = First' { getFirst' = Nada}

instance (Arbitrary a ) => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (3, return $ First' (Only a)),
                    (1, return $ First' Nada) ]


-- firstMappend :: First' a -> First' a -> First' a
-- firstMappend = mappend

type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftId :: First' String -> Bool)
    quickCheck (monoidRightId :: First' String -> Bool)


-- Our expected output demonstrates a different Monoid for Optional/Maybe
-- which is getting the first success and holding onto it, where any exist.
-- This could be seen, with a bit of hand-waving, as being like a
-- disjunctive or “or”-oriented Monoid instance.
-- Prelude> First' (Only 1) `mappend` First' Nada
-- First' {getFirst' = Only 1}
-- Prelude> First' Nada `mappend` First' Nada
-- First' {getFirst' = Nada}
-- Prelude> First' Nada `mappend` First' (Only 2)
-- First' {getFirst' = Only 2}
-- Prelude> First' (Only 1) `mappend` First' (Only 2)
-- First' {getFirst' = Only 1}