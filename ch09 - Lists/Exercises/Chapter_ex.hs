module Chapter_ex where
import Data.Char
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