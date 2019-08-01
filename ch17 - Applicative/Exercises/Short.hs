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
summed =  sum $ (,) x y