module Chapter_ex where
import Data.Char
import Data.Bool
---------------
-- Data.Char
---------------

-- 1. Query the types of isUpper and toUpper .
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2. Given the following behaviors, which would we use to write
-- a function that filters all the uppercase letters out of a String ?
-- Write that function such that, given the input “HbEfLrLxO,” your
-- function will return “HELLO.”
-- Prelude Data.Char> isUpper 'J'
-- True
-- Prelude Data.Char> toUpper 'j'
-- 'J'
uppercase :: [Char] -> [Char]
uppercase s = filter (\x -> isUpper x) s

-- 3. Write a function that will capitalize the first letter of a String
-- and return the entire String. For example, if given the argument
-- “julie,” it will return “Julie.”
capitalize :: [Char] -> [Char]
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

-- 4. Now make a new version of that function that is recursive such
-- that if you give it the input “woot” it will holler back at you
-- “WOOT.” The type signature won’t change, but you will want to
-- add a base case.
capitalizeAll :: [Char] -> [Char]
capitalizeAll "" = ""
capitalizeAll (x : xs) = toUpper x : capitalizeAll xs

-- 5. To do the final exercise in this section, we’ll need another stan-
-- dard function for lists called head . Query the type of head and experiment
-- with it to see what it does. Now write a function that
-- will capitalize the first letter of a String and return only that letter
-- as the result.
-- head :: [a] -> a
capitalizeFirst :: [Char] -> Char
capitalizeFirst "" = ' '
capitalizeFirst (x : _) = toUpper x


-- 6. Cool. Good work. Now rewrite it as a composed function. Then,
-- for fun, rewrite it pointfree.
capitalizeFirstComp :: [Char] -> Char
capitalizeFirstComp "" = ' '
capitalizeFirstComp s = toUpper . head $ s

capitalizeFirstPF :: [Char] -> Char
capitalizeFirstPF = toUpper . head

----------------------------------------
-- Writing your own standard functions
----------------------------------------

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs
-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd xs


-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
-- myOr = undefined
myOr [] = False
myOr (x:xs) = x || myOr xs


-- 2. myAny returns True if a -> Bool applied to any of the values in
-- the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
-- myAny = undefined
-- myAny f l = myOr . map f $ l
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

-- Example for validating myAny:
-- Prelude> myAny even [1, 3, 5]
-- False
-- Prelude> myAny odd [1, 3, 5]
-- True


-- 3. After you write the recursive myElem, write another version that
-- uses any.
-- -- the built-in version of 'elem' in GHC 7.10
-- -- and newer has a type that uses Foldable
-- -- instead of the list type specifically. You
-- -- can ignore that and write the concrete
-- -- version that works only for list.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = if a == x then True else myElem a xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a = myAny (== a)

-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False


-- 4. Implement myReverse.
myReverse :: [a] -> [a]
-- myReverse = undefined
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Prelude> myReverse "blah"
-- "halb"
-- Prelude> myReverse [1..5]
-- [5,4,3,2,1]


-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
-- squish = undefined
squish [] = []
squish (x:xs) = x ++ squish xs


-- 6. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap = undefined
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- Prelude> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
-- "WO 1 HOO WO 2 HOO WO 3 HOO "


-- 7. squishAgain flattens a list of lists into a list. This time re-use
-- the squishMap function.
squishAgain :: [[a]] -> [a]
-- squishAgain = undefined
squishAgain = squishMap id

-- 8. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
-- -- If you import maximumBy from Data.List,
-- -- you'll see the type is
-- -- Foldable t => (a -> a -> Ordering) -> t a -> a
-- -- rather than
-- -- (a -> a -> Ordering) -> [a] -> a
-- -- if you have GHC 7.10 or newer. Seeing a pattern?
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = if(f x mmb == GT ) then x else mmb
    where mmb = myMaximumBy f xs

-- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- 10
-- Prelude> myMaximumBy compare [1..10]
-- 10


-- 9. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the comparison
-- returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = if(f x mmb == LT ) then x else mmb
    where mmb = myMaximumBy f xs