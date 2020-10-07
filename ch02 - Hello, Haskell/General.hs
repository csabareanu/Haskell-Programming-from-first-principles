-- REPL (Read-Eval-Print-Loop)

-- OR load a file in the REPL with :load name_of_the_file
-- Obs. To unload a file from the REPL use :m
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

-- Everything in Haskell is an expression or declaration.

-- FUNCTIONS
-- In Haskell, as in Lambda Calculus, functions take only an argument and return one result (currying)
-- Used to abstract parts of code we want to reuse.

-- triple  x   =  x * 3
-- -- [1] [2] [3] [ 4 ]

-- 1. Name of the function we are defining. Note that it is lowercase.
-- 2. Argument to our function. The arguments to our function correspond
-- to the “head” of a lambda.
-- 3. The = is used to define (or declare) values and functions. Reminder:
-- this is not how we express equality between two values
-- in Haskell.
-- 4. Body of the function, an expression that could be evaluated if the
-- function is applied to a value. What triple is applied to will be
-- the value which the argument 𝑥 is bound to. Here the expression
-- x * 3 constitutes the body of the function. So, if you have an
-- expression like triple 6, 𝑥 is bound to 6. Since you’ve applied
-- the function, you can also replace the fully applied function with
-- its body and bound arguments.

-- Obs. Module names, types start with capital letter
--      Function names and variables start with lowercase letters

-----------------------
-- OTHER CONVENTIONS
-----------------------
-- Type variables: start at "a" and so on ...
-- Functions as arguments: f, g and so on...
-- Arguments to functions: x, y and so on...

----------------------------
-- Intermission: Exercises
----------------------------
-- 1. Given the following lines of code as they might appear in a
-- source file, how would you change them to use them directly in
-- the REPL?
-- half x = x / 2
-- square x = x * x
-- R:
-- let half x = x / 2
-- let square x = x * x

-- 2. Write one function that can accept one argument and work for
-- all the following expressions. Be sure to name the function.
-- 3.14 * (5 * 5)
-- 3.14 * (10 * 10)
-- 3.14 * (2 * 2)
-- 3.14 * (4 * 4)
-- R:
-- multiply x = 3.14 * (x * x)

--------------------
-- INFIX OPERATORS
--------------------
-- Default is prefix syntax but some functions as arithmetic operators are infix
-- Operators can be left associative (infixl) or right associative (infixr - exponentiation)
-- id 1 vs 1 + 1
-- You can use functions in infix or prefix style:
-- Ex. 10 `div` 4 vs div 10 4


----------------------------
-- Intermission: Exercises
----------------------------
-- Below are some pairs of functions that are alike except for parenthesization.
-- Read them carefully and decide if the parentheses change
-- the results of the function. Check your work in GHCi.
-- 1. a) 8 + 7 * 9
-- b) (8 + 7) * 9
-- R: YES

-- 2. a) perimeter x y = (x * 2) + (y * 2)
-- b) perimeter x y = x * 2 + y * 2
-- R: NO

-- 3. a) f x = x / 2 + 9
-- b) f x = x / (2 + 9)
-- R: YES


---------------------
-- Declaring values
---------------------
-- In a source code the order of declaration does not matter because ghci loads the entire file at once
-- In REPL the order of declaration does matter
-- Indentation does matter

----------------------------
-- Intermission: Exercises
----------------------------
-- The following code samples are broken and won’t compile. The first
-- two are as you might enter into the REPL; the third is from a source
-- file. Find the mistakes and fix them so that they will.
-- 1. let area x = 3. 14 * (x * x)
--let area x = 3.14 * (x*x)

-- 2. let double x = b * 2
-- Variable not in scope (b)
-- let double x = x * 2

-- 3. x = 7
--     y = 10
--    f = x + y
x = 7
y = 10
f = x + y


-- The negation of numbers (-) in Haskell is a form of syntactic sugar. There are 2 possible interpretations:
-- 1) an alias for negate
-- 2) subtraction function








