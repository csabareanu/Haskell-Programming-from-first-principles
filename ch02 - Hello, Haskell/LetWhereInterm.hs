module LetWhereInterm where

-------------------------------
-- Intermission: Exercises
-------------------------------

-- Now for some exercises. First, determine in your head what the
-- following expressions will return, then validate in the REPL:
-- 1. let x = 5 in x                   -- 5
-- 2. let x = 5 in x * x               -- 25
-- 3. let x = 5; y = 6 in x * y        -- 30
-- 4. let x = 3; y = 1000 in x + 3     -- 6

-- Above, you entered some let expressions into your REPL to evaluate
-- them. Now, we’re going to open a file and rewrite some let
-- expressions into where clauses. You will have to give the value you’re
-- binding a name, although the name can be just a letter if you like. For
-- example,
-- -- this should work in GHCi
-- let x = 5; y = 6 in x * y
-- could be rewritten as
-- -- put this in a file
mult1 = x * y
    where x = 5
          y = 6

add1 = x + 3
    where x = 3
          y = 1000
-- Making the equals signs line up is a stylistic choice. As long as the
-- expressions are nested in that way, the equals signs do not have to line
-- up. But notice we use a name that we will use to refer to this value in
-- the REPL:
-- *Main> :l practice.hs
-- [1 of 1] Compiling Main
-- Ok, modules loaded: Main.
-- *Main> mult1
-- 30



-- Rewrite the following let expressions into declarations with where
-- clauses:
-- 1. let x = 3; y = 1000 in x * 3 + y
-- 2. let y = 10; x = 10 * 5 + y in x * 5
-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y

-- 1.
ex1 = x * 3 + y
    where x = 3
          y = 1000

ex2 = x * 5
    where y = 10
          x = 10 * 5 + y

ex3 = z / (x + y)
    where x = 7
          y = negate x
          z = y * 10