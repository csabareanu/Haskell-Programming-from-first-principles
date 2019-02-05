---------------------
-- Using QuickCheck
---------------------
import Data.List (sort)


Test some simple arithmetic properties using QuickCheck.
-- 1. -- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half

-- 2.
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)


-- 3. Now we’ll test the associative and commutative properties of
-- addition:
plusAssociative x y z =
    x + (y + z) == (x + y) + z
plusCommutative x y =
    x + y == y + x


-- 4. Now do the same for multiplication.


-- 5. We mentioned in one of the first chapters that there are some
-- laws involving the relationship of quot and rem and div and mod.
-- Write QuickCheck tests to prove them.
-- quot rem
(quot x y)*y + (rem x y) == x
(div x y)*y + (mod x y) == x



-- 6. Is (^) associative? Is it commutative? Use QuickCheck to see if
-- the computer can contradict such an assertion.



-- 7. Test that reversing a list twice is the same as the identity of the
-- list:
reverse . reverse == id



-- 8. Write a property for the definition of ($).
f $ a = f a
f . g = \x -> f (g x)



-- 9. See if these two functions are equal:
foldr (:) == (++)
foldr (++) [] == concat


-- 10. Hm. Is that so?
f n xs = length (take n xs) == n



-- 11. Finally, this is a fun one. You may remember we had you compose
-- read and show one time to complete a “round trip.” Well,
-- now you can test that it works:
f x = (read (show x)) == x


-----------
--Failure
-----------

-- Find out why this property fails.
-- for a function
square x = x * x
-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt
-- Hint: Read about floating point arithmetic and precision if you’re
-- unfamiliar with it.


---------------
--Idempotence
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
-- 1.
f x =
    capitalizeWord x
        == twice capitalizeWord x
        == fourTimes capitalizeWord x
-- 2.
f x =
    sort x
        == twice sort x
        == fourTimes sort x

-- Make a Gen random generator for the datatype
-- We demonstrated in the chapter how to make Gen generators for
-- different datatypes. We are so certain you enjoyed that, we are going
-- to ask you to do it for some new datatypes:
-- 1. Equal probabilities for each.
data Fool =
        Fulse
        | Frue
    deriving (Eq, Show)

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
data Fool =
        Fulse
        | Frue
    deriving (Eq, Show)