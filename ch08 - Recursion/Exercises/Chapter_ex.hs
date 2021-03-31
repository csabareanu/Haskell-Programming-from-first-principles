module Chapter_ex where

import Data.List (intercalate)

-------------------
-- Review of types
-------------------

-- 1. What is the type of [[True, False], [True, True], [False, True]]?
-- a) Bool
-- b) mostly True
-- c) [a]
-- d) [[Bool]]

-- d) [[Bool]]


-- 2. Which of the following has the same type as [[True, False], [True, True], [False, True]]?
-- a) [(True, False), (True, True), (False, True)]
-- b) [[3 == 3], [6 > 5], [3 < 4]]
-- c) [3 == 3, 6 > 5, 3 < 4]
-- d) ["Bool", "more Bool", "Booly Bool!"]

-- b) [[3 == 3], [6 > 5], [3 < 4]]


-- 3. For the following function
-- func :: [a] -> [a] -> [a]
-- func x y = x ++ y
-- which of the following is true?
-- a) x and y must be of the same type
-- b) x and y must both be lists
-- c) if x is a String then y must be a String
-- d) all of the above

-- d) all of the above


-- 4. For the func code above, which is a valid application of func to
-- both of its arguments?
-- a) func "Hello World"
-- b) func "Hello" "World"
-- c) func [1, 2, 3] "a, b, c"
-- d) func ["Hello", "World"]

-- b) func "Hello" "World"


-----------------------
-- Reviewing currying
-----------------------

-- Given the following definitions, tell us what value results from further
-- applications.
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- :t cattyConny "Catty" :: String -> String
-- :t cattyConny "Catty" "Conny" :: String


-- -- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. What is the value of appedCatty "woohoo!" ? Try to determine
-- the answer for yourself, then test in the REPL.
-- "woops mrow woohoo!"

-- 2. frappe "1"
-- "1 mrow hahaha"

-- 3. frappe (appedCatty "2")
-- "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue")
-- "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink")
--               (cattyConny "green" (appedCatty "blue"))
-- "pink mrow haha mrow green mrow woops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
-- "are mrow Pugs mrow awesome"


------------
--Recursion
------------

-- 1. Write out the steps for reducing dividedBy 15 2 to its final
-- answer according to the Haskell code.
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- dividedBy 15 2
-- go 15 2 0
-- go 13 2 1
-- go 11 2 2
-- go 9  2 3
-- go 7  2 4
-- go 5  2 5
-- go 3  2 6
-- go 1  2 7
-- (7, 1)

-- 2. Write a function that recursively sums all numbers from 1 to n,
-- n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 +
-- 5 to get 15. The type should be (Eq a, Num a) => a -> a.

sumRec :: (Eq a, Num a) => a -> a
sumRec n
    | n == 0    = 0
    | otherwise = n + sumRec (n - 1)

-- more complicated with a go function 
sumNum :: (Eq a, Num a) => a -> a
sumNum n = go 0 0 
    where 
            go c sums
                | c == n    = sums + c
                | otherwise = go (c + 1) (sums + c)  


-- 3. Write a function that multiplies two integral numbers using
-- recursive summation. The type should be (Integral a) => a
-- -> a -> a.

mult :: (Integral a) => a -> a -> a
mult x y
    | y == 0    = 0
    | otherwise = x + mult x (y - 1)


--------------------
-- Fixing dividedBy
--------------------

-- Our dividedBy function wasn’t quite ideal. For one thing. It was a
-- partial function and doesn’t return a result (bottom) when given a
-- divisor that is 0 or less.

data DividedResult
    = DividedByZero
    | Result (Integer, Integer)
    deriving Show

dividedBy2 :: Integer -> Integer -> DividedResult
dividedBy2 _   0     = DividedByZero
dividedBy2 num denom = go (abs num) (abs denom) 0
                    where go n d count
                            | n < d && num > 0 && denom > 0 = Result (count, n)
                            | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
                            | n < d && num < 0 && denom < 0 = Result (count, negate n)
                            | n < d && num > 0 && denom < 0 = Result (negate count, n)
                            | otherwise = go (n - d) d (count + 1)

--------------------------
-- McCarthy 91 function
--------------------------

-- We’re going to describe a function in English, then in math notation,
-- then show you what your function should return for some test inputs.
-- Your task is to write the function in Haskell.
-- The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91 otherwise.
-- The function is recursive.

mc91 :: Integral a => a -> a
mc91 x
    | x > 100   = x - 10
    | otherwise = mc91 . mc91 $ x + 11

-- mc91 77
-- mc91 $ mc91 88
-- mc91 $ mc91 $ mc91 99
-- mc91 $ mc91 $ mc91 $ mc91 110
-- mc91 $ mc91 $ mc91 100
-- mc91 $ mc91 $ mc91 $ mc91 111
-- mc91 $ mc91 $ mc91 101
-- mc91 $ mc91 91
-- mc91 $ mc91 $ mc91 102
-- mc91 $ mc91 92
-- .................
-- mc91 $ mc91 100
-- mc91 $ mc91 $ mc91 111
-- mc91 $ mc91 101
-- mc91 91
-- mc91 $ mc91 102
-- mc91 92
-- ..................
-- mc91 100
-- mc91 $ mc91 111
-- mc91 101
-- 91

----------------------
-- Numbers into words
----------------------

-- module WordNumber where


digitToWord :: Int -> String
digitToWord n = intercalate "-" $ map wordNumber $ digits n
-- or with function composition
-- digitToWord n = intercalate "-" . map wordNumber $ digits n

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

-- digits 123
-- digits 12 ++ [3]
-- digits 1 ++ [2] ++ [3]
-- digits 0 ++ [1] ++ [2] ++ [3]
-- [] ++ [1] ++ [2] ++ [3]
-- [1, 2, 3]


wordNumber :: Int -> String
-- wordNumber n = undefined
wordNumber n
    | n == 0         = "zero"
    | n == 1         = "one"
    | n == 2         = "two"
    | n == 3         = "three"
    | n == 4         = "four"
    | n == 5         = "five"
    | n == 6         = "six"
    | n == 7         = "seven"
    | n == 8         = "eight"
    | n == 9         = "nine"
    | otherwise = "NaN"


-- Here undefined is a placeholder to show you where you need to fill in
-- the functions. The n to the right of the function names is the argument
-- which will be an integer.
-- Fill in the implementations of the functions above so that wordNumber
-- returns the English word version of the Int value. You will first write a
-- function that turns integers from 1-9 into their corresponding English
-- words, ”one,” ”two,” and so on. Then you will write a function that
-- takes the integer, separates the digits, and returns it as a list of integers.
-- Finally you will need to apply the first function to the list produced by
-- the second function and turn it into a single string with interspersed
-- hyphens.
-- We’ve laid out multiple functions for you to consider as you tackle
-- the problem. You may not need all of them, depending on how you
-- solve it–these are just suggestions. Play with them and look up their
-- documentation to understand them in deeper detail.
-- You will probably find this difficult.
-- div :: Integral a => a -> a -> a
-- mod :: Integral a => a -> a -> a
-- map :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- intersperse :: a -> [a] -> [a]
-- (++) :: [a] -> [a] -> [a]
-- (:[]) :: a -> [a]

-- Here is what your output should look in the REPL when it’s working:
-- Prelude> wordNumber 12324546
-- "one-two-three-two-four-five-four-six"
-- Prelude>