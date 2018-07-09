module Eq_inst_interm where

-- Write the Eq instance for the datatype provided.

-- 1. It’s not a typo, we’re just being cute with the name.
-- data TisAnInteger =
-- TisAn Integer


data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'



-- 2. data TwoIntegers =
-- Two Integer Integer


data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'



-- 3. data StringOrInt =
-- TisAnInt Int
---- |TisAString String


data StringOrInt = TisAnInt Int | TisAsString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b)       = a == b
    (==) (TisAsString a) (TisAsString b) = a == b
    (==) _ _                             = False



-- 4. data Pair a =
-- Pair a a


data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'



-- 5. data Tuple a b =
-- Tuple a b


data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'



-- 6. data Which a =
-- ThisOne a
---- | ThatOne a


data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _                     = False



    -- 7. data EitherOr a b =
-- Hello a
---- | Goodbye b

data EitherOr a b = Hello a | GoodBye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y)     = x == y
    (==) (GoodBye x) (GoodBye y) = x == y
    (==) _ _                     = False