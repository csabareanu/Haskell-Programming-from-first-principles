-- Types are a way of categorizing values.

-- Prelude> :type 'a'
-- 'a' :: Char
-- :: reads as 'has the type'
-- A type signature is a line of code that defines the types for a value, expression, or function.

-- Prelude> :type "Hello!"
-- "Hello!" :: [Char]
-- Obs. [] the square brackets are syntactic sugar for a list
-- [Char] -> List of Char

-- module Print1 where

-- main :: IO ()
-- main = putStrLn "hello world!"

-- IO () is a type used by Haskell when the result of running a function results in side effects (not pure functions or expressions)
-- Prelude> putStrLn "hello world!"
-- hello world!

-----------------
-- Concatenation
-----------------
-- We concatenate strings with (++) and concat.

-- *Main> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- Obs. a inside the list type constructor, is a type variable
--      This type variable a is polymorphic

-- *Main> :t concat
-- concat :: Foldable t => t [a] -> [a]  <=> [[a]] -> [a]

------------------
-- Intermission
------------------

-- Read the syntax of the following functions and decide whether it will
-- compile. Test them in your REPL and try to fix the syntax errors
-- where they occur.
-- 1. ++ [1, 2, 3] [4, 5, 6]
-- R: No, ++ is an infix operator and when used in the prefix form must by used with paranthesis ().

-- 2. '<3' ++ ' Haskell'
-- R: No, double quotes must be used since this are list of chars (~Strings)

-- 3. concat ["<3", " Haskell"]
-- R: Yes

--------------------------
-- More list functions
---------------------------
-- most of the functions for manipulating lists work on strings as well, because a string is just a list of chars [Char]
-- head, tail, (:), take, drop, !!




