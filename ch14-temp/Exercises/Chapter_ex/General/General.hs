---------------------
-- Using QuickCheck
---------------------
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

----------------------------------------------------------------
-- Test some simple arithmetic properties using QuickCheck.
----------------------------------------------------------------

-- 1. -- for a function
half x = x / 2
-- this property should hold

halfIdentity = (*2) . half

prop_halfId :: Double -> Bool
prop_halfId x = halfIdentity x == (x::Double)

check_prop_halfId :: IO ()
check_prop_halfId = quickCheck prop_halfId

-- 2.
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered l = listOrdered . sort $ (l :: [Int])

prop_listOrdered_string :: String -> Bool
prop_listOrdered_string l = listOrdered . sort $ (l :: String)


check_prop_listOrdered :: IO ()
check_prop_listOrdered = quickCheck prop_listOrdered

check_prop_listOrdered_string :: IO ()
check_prop_listOrdered_string = quickCheck prop_listOrdered_string


-- 3. Now we’ll test the associative and commutative properties of
-- addition:
plusAssociative x y z =
    x + (y + z) == (x + y) + z

check_prop_assoc :: IO ()
check_prop_assoc = quickCheck (plusAssociative :: Int->Int->Int->Bool)

plusCommutative x y =
    x + y == y + x

check_prop_comm :: IO ()
check_prop_comm = quickCheck (plusCommutative :: Int->Int->Bool)



-- -- 4. Now do the same for multiplication.
multAssoc a b c =
    a * (b * c) == (a * b) * c

multComm a b =
    a * b == b * a

check_mult_assoc :: IO()
check_mult_assoc = quickCheck (multAssoc :: Int->Int->Int->Bool)

check_mult_comm :: IO()
check_mult_comm = quickCheck (multComm :: Int->Int->Bool)


-- 5. We mentioned in one of the first chapters that there are some
-- laws involving the relationship of quot and rem and div and mod.
-- Write QuickCheck tests to prove them.
-- quot rem
-- (quot x y)*y + (rem x y) == x
-- (div x y)*y + (mod x y) == x

prop_quotrem1 :: Int -> NonZero Int -> Bool
prop_quotrem1 x (NonZero y) = (quot x y) * y + (rem x y) == x

prop_quotrem2 :: Int -> NonZero Int -> Bool
prop_quotrem2 x (NonZero y) = (div x y) * y + (mod x y) == x

check_quotrem1 :: IO()
check_quotrem1 = quickCheck prop_quotrem1

check_quotrem2 :: IO()
check_quotrem2 = quickCheck prop_quotrem2


-- -- 6. Is (^) associative? Is it commutative? Use QuickCheck to see if
-- -- the computer can contradict such an assertion.

prop_pow_assoc :: Int -> Int -> Int-> Bool
prop_pow_assoc a b c = (a^b)^c == a^(b^c)

prop_pow_comm :: Int -> Int -> Bool
prop_pow_comm a b = (a^b) == (b^a)

check_pow_assoc :: IO()
check_pow_assoc = quickCheck prop_pow_assoc

check_pow_comm :: IO()
check_pow_comm = quickCheck prop_pow_comm


-- -- 7. Test that reversing a list twice is the same as the identity of the
-- -- list:
-- -- reverse . reverse == id
prop_2_reverse :: [Int] -> Bool
prop_2_reverse xs = (==) (id xs) (reverse . reverse $ xs)


check_2_reverse :: IO()
check_2_reverse = quickCheck prop_2_reverse


-- 8. Write a property for the definition of ($).
-- f $ a = f a
-- f . g = \x -> f (g x)

prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f x = f x == (f $ x)

check_dollar_1 :: IO()
check_dollar_1 = quickCheck (prop_dollar id :: Int -> Bool)

check_dollar_2 :: IO()
check_dollar_2 = quickCheck (prop_dollar length :: [Int] -> Bool)



-- 9. See if these two functions are equal:
-- foldr (:) == (++)
-- foldr (++) [] == concat

prop_cons :: [Int] -> [Int] -> Bool
prop_cons x y = foldr (:) x y == (++) y x

prop_concat :: [[Int]] -> Bool
prop_concat x = foldr (++) [] x == concat x

-- passes the test only if we compare foldr (:) x y with (++) y x.
cons_test :: IO()
cons_test = quickCheck prop_cons

concat_test :: IO()
concat_test = quickCheck prop_concat



-- 10. Hm. Is that so?
-- f n xs = length (take n xs) == n
-- holds only if n <= length xs
prop_len :: Int -> [Int] -> Bool
prop_len n xs = length (take n xs) == n

len_test :: IO()
len_test = quickCheck prop_len


-- 11. Finally, this is a fun one. You may remember we had you compose
-- read and show one time to complete a “round trip.” Well,
-- now you can test that it works:
-- f x = (read (show x)) == x

prop_rs :: [Int] -> Bool
prop_rs x = read . show $ x == x

rs_test :: IO()
rs_test = quickCheck prop_rs


-- -----------
-- --Failure
-- -----------

-- Find out why this property fails.
-- for a function
square :: (Num a) => a -> a
square = (^2)
-- why does this property not hold? Examine the type of sqrt.
-- squareIdentity = square . sqrt
prop_sq :: (Eq a, Floating a) => a -> Bool
prop_sq x = (==) x $ square . sqrt $ x

sq_test :: IO ()
sq_test = quickCheck (prop_sq :: Float -> Bool)

-- Hint: Read about floating point arithmetic and precision if you’re
-- unfamiliar with it.
-- Response: because of floating point precision issues


-- ---------------
-- --Idempotence
-- ---------------

-- -- Idempotence refers to a property of some functions in which the
-- -- result value does not change beyond the initial application. If you
-- -- apply the function once, it returns a result, and applying the same
-- -- function to that value won’t ever change it. You might think of a list
-- -- that you sort: once you sort it, the sorted list will remain the same
-- -- after applying the same sorting function to it. It’s already sorted, so
-- -- new applications of the sort function won’t change it.
-- -- Use QuickCheck and the following helper functions to demonstrate
-- -- idempotence for the following:
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord w = map toUpper (take 1 w) ++ (drop 1 w)


-- -- 1.
-- f x =
--     capitalizeWord x
--         == twice capitalizeWord x
--         == fourTimes capitalizeWord x
prop_id :: String -> Bool
prop_id s = cap == twice capitalizeWord s && cap == fourTimes capitalizeWord s
    where cap = capitalizeWord s

id_test :: IO()
id_test = quickCheck prop_id

-- -- 2.
-- f x =
--     sort x
--         == twice sort x
--         == fourTimes sort x

prop_sort :: [Int] -> Bool
prop_sort x = srt == twice sort x && srt == fourTimes sort x
    where srt = sort x

sort_test :: IO()
sort_test = quickCheck prop_sort

-- -- Make a Gen random generator for the datatype
-- -- We demonstrated in the chapter how to make Gen generators for
-- -- different datatypes. We are so certain you enjoyed that, we are going
-- -- to ask you to do it for some new datatypes:
-- -- 1. Equal probabilities for each.
-- data Fool =
--         Fulse
----         | Frue
--     deriving (Eq, Show)

-- -- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
-- -- data Fool =
-- --         Fulse
-- --         | Frue
-- --     deriving (Eq, Show)