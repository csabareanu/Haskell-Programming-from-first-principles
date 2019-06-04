{-# LANGUAGE ViewPatterns #-}

module QuickCheckFunctions where

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) =>
                        f a
                    -> Fun a b
                    -> Fun b c
                    -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- *QuickCheckFunctions> type IntToInt = Fun Int Int
-- *QuickCheckFunctions> type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
-- *QuickCheckFunctions> quickCheck (functorCompose' :: IntFC)
-- +++ OK, passed 100 tests.