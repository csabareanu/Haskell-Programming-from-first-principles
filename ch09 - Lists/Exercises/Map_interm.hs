module Map_interm where

-- 1. Will the following expression return a value or be ⊥?
-- take 1 $ map (+1) [undefined, 2, 3]

-- Will be bottom


-- 2. Will the following expression return a value?
-- take 1 $ map (+1) [1, undefined, 3]

-- //returns [2]


-- 3. take 2 $ map (+1) [1, undefined, 3]

-- Will be bottom


-- 4. What does the following mystery function do? What is its type?
-- Describe it (to yourself or a loved one) in standard English and
-- then test it out in the REPL to make sure you were correct.
-- itIsMystery xs = map (\x -> elem x "aeiou") xs

-- determines if a letter is a vowel or not
-- itIsMystery :: [Char] -> [Bool]


-- 5. What will be the result of the following functions:
-- a) map (^2) [1..10]
-- a) [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- b) map minimum [[1..10], [10..20], [20..30]]
-- -- n.b. `minimum` is not the same function
-- -- as the `min` that we used before
-- b) [1, 10 , 20]

-- c) map sum [[1..5], [1..5], [1..5]]
-- c) [15, 15, 15]


-- 6. Back in the Functions chapter, you wrote a function called fold-
-- Bool. That function exists in a module known as Data.Bool and
-- is called bool. Write a function that does the same (or similar, if
-- you wish) as the map (if-then-else) function you saw above
-- but uses bool instead of the if-then-else syntax. Your first
-- step should be bringing the bool function into scope by typing
-- import Data.Bool at your Prelude prompt.

-- map (\x -> bool (x) (-x) (x == 3)) [1..10])