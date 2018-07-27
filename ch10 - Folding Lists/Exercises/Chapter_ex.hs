module Chapter_ex where

import Data.Bool

------------------------
-- Warm-up and review
------------------------

-- For the following set of exercises, you are not expected to use folds.
-- These are intended to review material from previous chapters. Feel
-- free to use any syntax or structure from previous chapters that seems
-- appropriate.
-- 1. Given the following sets of consonants and vowels:
-- stops = "pbtdkg"
-- vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels
-- and makes 3-tuples of all possible stop-vowel-stop combinations.
-- These will not all correspond to real words in English,
-- although the stop-vowel-stop pattern is common enough
-- that many of them will.
words_tuples :: [(Char, Char, Char)]
words_tuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
    where stops  = "pbtdkg"
          vowels = "aeiou"

-- b) Modify that function so that it only returns the combinations
-- that begin with a p.
words_tuples_p ::[(Char, Char, Char)]
words_tuples_p = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']
    where stops  = "pbtdkg"
          vowels = "aeiou"

-- c) Now set up lists of nouns and verbs (instead of stops and
-- vowels) and modify the function to make tuples representing
-- possible noun-verb-noun sentences.
nouns :: [[Char]]
nouns = ["bike", "cat", "mountain", "teacup", "bear", "car"]

verbs :: [[Char]]
verbs = ["eat", "walk", "run", "see", "work", "drive", "smell"]

sentences :: [([Char], [Char], [Char])]
sentences = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. What does the following mystery function do? What is its type?
-- Try to get a good sense of what it does before you test it in the
-- REPL to verify it.
seekritFunc :: [Char] -> Int
seekritFunc x =
    div (sum (map length (words x)))
    (length (words x))

-- medium number of letters per word in a sentence

-- 3. We’d really like the answer to be more precise. Can you rewrite
-- that using fractional division?
seekritFuncPrec :: [Char] -> Double
seekritFuncPrec x =
     (fromIntegral (sum (map length (words x)))) /
        (fromIntegral (length (words x)))

-------------------------
-- Prime number machine
-------------------------

-- Rewriting functions using folds
-- In the previous chapter, you wrote these functions using direct recursion
-- over lists. The goal now is to rewrite them using folds. Where
-- possible, to gain a deeper understanding of folding, try rewriting the
-- fold version so that it is point-free.

-- Point-free versions of these functions written with a fold should look
-- like:
-- myFunc = foldr f z
-- So for example with the and function:
-- Again, this type will be less reusable than
-- the one in GHC 7.10 and newer. Don't worry.

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
    then False
    else myAnd xs
-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd2 xs
-- fold, not point-free in the folding function
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr
    (\a b ->
        if a == False
        then False
        else b) True
-- fold, both myAnd and the folding function are point-free now
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr (&&) True

-- The goal here is to converge on the final version where possible. You
-- don’t need to write all variations for each example, but the more
-- variations you write, the deeper your understanding of these functions
-- will become.
-- foldr f acc (x:xs) = f x (foldr f acc xs)

-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
-- myOr = undefined
myOr = foldr (||) False
-- myOr [False, True, False]
-- (||) False (foldr (||) False [True, False])
-- (||) False ((||) True (foldr (||) False [False]))
-- (||) False ((||) True ((||) False (foldr (||) False [])))
-- (||) False ((||) True ((||) False False))
-- (||) False ((||) True False)
-- (||) False True
-- True

-- 2. myAny returns True if a -> Bool applied to any of the values in
-- the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
-- myAny = undefined
-- with map
myAny f = myOr . map f
-- only with fold
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x y -> if f x then True else y) False
-- or :
myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f = foldr ((||).f) False
-- Example for validating myAny:
-- Prelude> myAny even [1, 3, 5]
-- False
-- Prelude> myAny odd [1, 3, 5]
-- True


-- 3. In addition to the recursive and fold based myElem, write a version
-- that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||).(== x)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = myAny'' (==x)
-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False


-- 4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
-- myReverse = undefined
myReverse = foldl (flip (:)) []

-- Prelude> myReverse "blah"
-- "halb"
-- Prelude> myReverse [1..5]
-- [5,4,3,2,1]


-- 5. Write myMap in terms of foldr. It should have the same behavior
-- as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
-- myMap = undefined
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same
-- behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter = undefined
myFilter f = foldr (\a b -> if f a then a : b else b) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\a b -> bool b (a : b) (f a)) []


-- 7. squish flattens a list of lists into a list
-- squish :: [[a]] -> [a]
-- -- squish = undefined
-- squish
-- 8. squishMap maps a function over a list and concatenates the results.
-- squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap = undefined
-- Prelude> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- Prelude> squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah"
-- "WO b OT WO l OT WO a OT WO h OT "
-- 9. squishAgain flattens a list of lists into a list. This time re-use
-- the squishMap function.
-- squishAgain :: [[a]] -> [a]
-- squishAgain = undefined
-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
-- myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy = undefined
-- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- 10
-- Prelude> myMaximumBy compare [1..10]
-- 10
-- 11. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the comparison
-- returned LT for.
-- myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy = undefined
-- Prelude> myMinimumBy (\_ _ -> GT) [1..10]
-- 10
-- Prelude> myMinimumBy (\_ _ -> LT) [1..10]
-- 1
-- Prelude> myMinimumBy compare [1..10]
-- 1