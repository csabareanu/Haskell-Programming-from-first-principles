
--------------------
-- Using QuickCheck
--------------------
import Test.QuickCheck

import Data.List (sort)
import Data.Char (toUpper)

-- Test some simple arithmetic properties using QuickCheck.

-- 1. -- for a function
half x = x / 2
-- -- this property should hold
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == (x::Double)

test_halfIdentity :: IO ()
test_halfIdentity = quickCheck prop_halfIdentity


-- 2. import Data.List (sort)
-- -- for any list you apply sort to
-- -- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

test_listOrderedInt :: IO ()
test_listOrderedInt = quickCheck (listOrdered . sort :: [Int] -> Bool)

test_listOrderedStr :: IO ()
test_listOrderedStr = quickCheck (listOrdered . sort :: String -> Bool)


-- 3. Now we’ll test the associative and commutative properties of
-- addition:
plusAssociative x y z =
    x + (y + z) == (x + y) + z
plusCommutative x y =
    x + y == y + x

test_assoc :: IO ()
test_assoc = quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)

test_comm :: IO ()
test_comm = quickCheck (plusCommutative :: Int -> Int -> Bool)


-- 4. Now do the same for multiplication.

multAssociative x y z =
    x * (y * z) == (x * y) * z
multCommutative x y =
    x * y == y * x

test_assoc_mult :: IO ()
test_assoc_mult = quickCheck (multAssociative :: Int -> Int -> Int -> Bool)

test_comm_mult :: IO ()
test_comm_mult = quickCheck (multCommutative :: Int -> Int -> Bool)


-- 5. We mentioned in one of the first chapters that there are some
-- laws involving the relationship of quot and rem and div and mod.
-- Write QuickCheck tests to prove them.
-- -- quot rem
-- (quot x y)*y + (rem x y) == x
-- (div x y)*y + (mod x y) == x

prop_quot_rem :: Int -> NonZero Int -> Bool
prop_quot_rem x (NonZero y) = (quot x y) * y + (rem x y) == x

prop_div_mod :: Int -> NonZero Int -> Bool
prop_div_mod x (NonZero y) = (div x y) * y + (mod x y) == x

test_prop_quot_rem :: IO ()
test_prop_quot_rem = quickCheck prop_quot_rem

test_prop_div_mod :: IO ()
test_prop_div_mod = quickCheck prop_div_mod

-- 6. Is (^) associative? Is it commutative? Use QuickCheck to see if
-- the computer can contradict such an assertion.

-- Associative:
-- (x ^ y) ^ z == x ^ (y ^ z)
-- Commutative:
-- x ^ y == y ^ x

prop_comm_pow :: Int -> Int -> Bool
prop_comm_pow x y = x ^ y == y ^ x

prop_assoc_pow :: Int -> Int -> Int -> Bool
prop_assoc_pow x y z = x ^ (y ^ z) == (x ^ y) ^ z

test_comm_pow :: IO ()
test_comm_pow = quickCheck prop_comm_pow

test_assoc_pow :: IO ()
test_assoc_pow = quickCheck prop_assoc_pow


-- 7. Test that reversing a list twice is the same as the identity of the
-- list:
-- reverse . reverse == id

prop_double_reverse :: [Int] -> Bool
prop_double_reverse xs = (== id xs) . reverse . reverse $ xs

test_double_reverse :: IO ()
test_double_reverse = quickCheck prop_double_reverse

--{-# ANN module "HLint: ignore Redundant $" #-}
-- 8. Write a property for the definition of ($).
-- f $ a = f a
-- f . g = \x -> f (g x)

prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f x = f x == (f $ x )

prop_dollar_test1 :: IO ()
prop_dollar_test1 = quickCheck (prop_dollar id :: Int -> Bool)

prop_dollar_test2 :: IO ()
prop_dollar_test2 = quickCheck (prop_dollar length :: String -> Bool)

prop_compose :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = f(g(x)) == (f . g) x

prop_compose_test1 :: IO ()
prop_compose_test1 = quickCheck (prop_compose (+1) (*5) :: Int -> Bool)

prop_compose_test2 :: IO ()
prop_compose_test2 = quickCheck (prop_compose id reverse :: String -> Bool)

prop_compose_test3 :: IO ()
prop_compose_test3 = quickCheck (prop_compose (==0) length :: [Int] -> Bool)


-- 9. See if these two functions are equal:
-- foldr (:) == (++)
-- foldr (++) [] == concat

prop_foldr :: (Eq a) => [a] -> [a] -> Bool
prop_foldr x y = foldr (:) x y == (++) x y

prop_foldr_test :: IO ()
prop_foldr_test = quickCheck (prop_foldr :: String -> String -> Bool)

prop_foldr_concat :: (Eq a) => [[a]] -> Bool
prop_foldr_concat xs = foldr (++) [] xs == concat xs

prop_foldr_concat_test :: IO ()
prop_foldr_concat_test = quickCheck (prop_foldr_concat :: [String] -> Bool)

prop_foldr_concat_test2 :: IO ()
prop_foldr_concat_test2 = quickCheck (prop_foldr_concat :: [[Int]] -> Bool)

-- 10. Hm. Is that so?
-- f n xs = length (take n xs) == n

-- this will be successfull if n < length xs
prop_length :: Int -> [a] -> Bool
prop_length n xs = length (take n xs) == n

prop_length_test :: IO ()
prop_length_test = quickCheck (prop_length :: Int -> [Int] -> Bool)


-- 11. Finally, this is a fun one. You may remember we had you compose
-- read and show one time to complete a “round trip.” Well,
-- now you can test that it works:
-- f x = (read (show x)) == x

prop_read_show :: (Eq a, Read a, Show a) => a -> Bool
prop_read_show x = read (show x) == x

prop_read_show_test :: IO ()
prop_read_show_test = quickCheck (prop_read_show :: Int -> Bool)

-----------
-- Failure
-----------

-- Find out why this property fails.
-- -- for a function
-- square x = x * x
-- -- why does this property not hold? Examine the type of sqrt.
-- squareIdentity = square . sqrt
-- Hint: Read about floating point arithmetic and precision if you’re
-- unfamiliar with it.
-- Response: because of floating point precision issues


square :: (Num a) => a -> a
square = (^2)

prop_square :: (Num a, Eq a, Floating a) => a -> Bool
prop_square x = (== x) . square . sqrt $ x

prop_square_test :: IO ()
prop_square_test = quickCheck (prop_square :: Float -> Bool)


---------------
-- Idempotence
---------------

-- Idempotence refers to a property of some functions in which the
-- result value does not change beyond the initial application. If you
-- apply the function once, it returns a result, and applying the same
-- function to that value won’t ever change it. You might think of a list
-- that you sort: once you sort it, the sorted list will remain the same
-- after applying the same sorting function to it. It’s already sorted, so
-- new applications of the sort function won’t change it.
-- Use QuickCheck and the following helper functions to demonstrate
-- idempotence for the following:
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord xs = map toUpper (take 1 xs) ++ drop 1 xs

-- 1.
-- f x = capitalize == twice capitalize && capitalize == fourTimes capitalize
--     where capitalize = capitalizeWord x




-- 2. f x =
-- sort x
-- == twice sort x
-- == fourTimes sort x


------------------------------------------------
-- Make a Gen random generator for the datatype
------------------------------------------------

-- We demonstrated in the chapter how to make Gen generators for
-- different datatypes. We are so certain you enjoyed that, we are going
-- to ask you to do it for some new datatypes:

-- 1. Equal probabilities for each.
-- data Fool =
-- Fulse
---- | Frue
-- deriving (Eq, Show)

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
-- data Fool =
-- Fulse
---- | Frue
-- deriving (Eq, Show)