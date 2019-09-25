module ZipList where

-- the default mappend for lists does the following:
-- [1, 2, 3] <> [4, 5, 6]
-- -- changes to
-- [1, 2, 3] ++ [4, 5, 6]
-- [1, 2, 3, 4, 5, 6]

-- A ZipList Monoid exists. It combines the values of the 2 lists as parallel sequences using a monoid provided by the values themselves.
-- [1, 2, 3] <> [4, 5, 6]
-- -- changes to
-- [
-- 1 <> 4
-- , 2 <> 5
-- , 3 <> 6
-- ]

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
    mempty  = pure mempty
    mappend = (<>)

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq



main :: IO ()
main = quickBatch (monoid (ZipList [1 :: Sum Int]))