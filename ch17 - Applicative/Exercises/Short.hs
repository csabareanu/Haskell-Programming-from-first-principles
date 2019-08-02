module Short where

import Data.List (elemIndex)

------------------------------------------------------------------------------
-- In the following exercises you will need to use the following terms to
-- make the expressions type-check:
-- 1. pure
-- 2. (<$>)
-- -- or fmap
-- 3. (<*>)
------------------------------------------------------------------------------

-- Make the following expressions type-check.
-- 1.
added :: Maybe Integer
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) -- Just 9


-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6] -- Just 6

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6] -- Just 5

tupled :: Maybe (Integer, Integer) -- Just (6,5)
-- tupled = (,) y z
tupled = fmap (,) y <*> z
-- *Short> Just ((,) 6) <*> (Just 5)
-- Just (6,5)
--tupled' :: Maybe (Integer, Integer)
tupled' = pure (,) <*> y <*> z
-- fmap f x = pure f <*> x



-- 3.
-- import Data.List (elemIndex)
xx :: Maybe Int
xx = elemIndex 3 [1, 2, 3, 4, 5]  -- Just 2
yy :: Maybe Int
yy = elemIndex 4 [1, 2, 3, 4, 5]  -- Just 3
max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
-- maxed = max' x y
maxed = fmap max' xx <*> yy

-- 4.
xs = [1, 2, 3]
ys = [4, 5, 6]
xxx :: Maybe Integer
xxx = lookup 3 $ zip xs ys  -- Just 6
yyy :: Maybe Integer
yyy = lookup 2 $ zip xs ys  -- Just 5

-- summed :: Maybe Integer
-- summed = sum $ (,) x y
-- summed = fmap sum $ (,) xxx <*> yyy
summed =  fmap sum $ fmap (,) xxx <*> yyy


----------------------------------------------------

-- const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
-- Identity [1,2,3]
-- Write an Applicative instance for Identity.

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure                            = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)


-------------------------------------------------------
-- *Main> Constant (Sum 1) <*> Constant (Sum 2)
-- Constant {getConstant = Sum {getSum = 3}

-- Prelude> Constant undefined <*> Constant (Sum 2)
-- Constant (Sum {getSum = *** Exception: Prelude.undefined

-- *Main> pure 1
-- 1
-- *Main> pure 1 :: Constant String Int
-- Constant {getConstant = ""}

-- Write an Applicative instance for Constant.
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x)  = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (a <> b)