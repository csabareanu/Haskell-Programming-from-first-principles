module Chapter_ex where

-- The goal for all the following exercises is just to get you playing with
-- code and forming hypotheses about what it should do. Read the code
-- carefully, using what we’ve learned so far. Generate a hypothesis
-- about what you think the code will do. Play with it in the REPL and
-- find out where you were right or wrong.
--------------------------
-- Parenthesization
--------------------------

-- Here we’ve listed the information that GHCi gives us for various infix
-- operators. We have left the type signatures in this time, although it is
-- not directly relevant to the following exercises. This will give you a
-- chance to look at the types if you’re curious and also provide a more
-- accurate picture of the :info command.
-- Prelude> :info (^)
-- (^) :: (Num a, Integral b) => a -> b -> a
-- infixr 8 ^
-- Prelude> :info (*)
-- class Num a where
-- (*) :: a -> a -> a
-- infixl 7 *
-- Prelude> :info (+)
-- class Num a where
-- (+) :: a -> a -> a
-- infixl 6 +
-- Prelude> :info (-)
-- class Num a where
-- (-) :: a -> a -> a
-- infixl 6 -
-- Prelude> :info ($)
-- ($) :: (a -> b) -> a -> b
-- infixr 0 $

-- We should take a moment to explain and demonstrate the ($) operator
-- as you will run into it fairly frequently in Haskell code. The good
-- news is it does almost nothing. The bad news is this fact sometimes
-- trips people up.
-- First, here’s the definition of ($):
-- f $ a = f a
-- Immediately this seems a bit pointless until we remember that it’s
-- defined as an infix operator with the lowest possible precedence. The
-- ($) operator is a convenience for when you want to express something
-- with fewer pairs of parentheses.
-- Example of ($) in some expressions:
-- Prelude> (2^) $ 2 + 2
-- 16
-- Prelude> (2^) (2 + 2)
-- 16
-- Prelude> (2^) 2 + 2
-- 6
-----------------------------------------------------------------------------
-- If you like, a way to understand ($) in words is: “evaluate everything
-- to the right of me first.”
-------------------------------------------------------------------------------
-- Also note that you can stack up multiple uses of ($) in the same
-- expression. For example, this works:
-- Prelude> (2^) $ (+2) $ 3*2
-- 256
-- But this does not:
-- Prelude> (2^) $ 2 + 2 $ (*30)
-- -- A rather long and ugly type error about trying to
-- -- use numbers as if they were functions follows.
-- We can see for ourselves why this code doesn’t make sense if we
-- examine the reduction steps.
-- -- Remember ($)'s definition
-- f $ a = f a
-- (2^) $ 2 + 2 $ (*30)
-- -- Given the right-associativity (infixr) of $
-- -- we must begin at the right-most position.
-- 2 + 2 $ (*30)
-- -- reduce ($)
-- (2 + 2) (*30)
-- -- then we must evaluate (2 + 2) before we can apply it
-- 4 (*30)
-- -- This doesn't make sense, we can't apply 4
-- -- as if it was a function to the argument (*30)!
-- Now let’s flip that expression around a bit so it works and then walk
-- through a reduction:
-- (2^) $ (*30) $ 2 + 2
-- -- must evaluate right-side first
-- (2^) $ (*30) $ 2 + 2
-- -- application of the function (*30) to the
-- -- expression (2 + 2) forces evaluation
-- (2^) $ (*30) 4
-- -- then we reduce (*30) 4
-- (2^) $ 120
-- -- reduce ($) again.
-- (2^) 120
-- -- reduce (2^)
-- 1329227995784915872903807060280344576
-- Given what we know about the precedence of (*), (+), and (^), how
-- can we parenthesize the following expressions more explicitly without
-- changing their results? Put together an answer you think is correct,
-- then test in the GHCi REPL.
-- Example:
-- -- We want to make this more explicit
-- 2 + 2 * 3 - 3
-- -- this will produce the same result
-- 2 + (2 * 3) - 3
-- Attempt the above on the following expressions.
-- 1. 2 + 2 * 3 - 1
-- R: 2 + (2 * 3) -1

-- 2. (^) 10 $ 1 + 1
-- R: 10 ^ (1 + 1)

-- 3. 2 ^ 2 * 4 ^ 5 + 1
-- R: (2 ^ 2) * (4 ^ 5) + 1

-- Equivalent expressions
-- Which of the following pairs of expressions will return the same result
-- when evaluated? Try to reason them out in your head by reading the
-- code and then enter them into the REPL to check your work:
-- 1. 1 + 1
-- 2
-- YES

-- 2. 10 ^ 2
-- 10 + 9 * 10
-- YES

-- 3. 400 - 37
-- (-) 37 400
-- NO

-- 4. 100 `div` 3
-- 100 / 3
-- NO

-- 5. 2 * 5 + 18
-- 2 * (5 + 18)
-- NO

-- More fun with functions
-- Here is a bit of code as it might be entered into a source file. Remember
-- that when you write code in a source file, the order is unimportant, but
-- when writing code directly into the REPL the order does matter. Given
-- that, look at this code and rewrite it such that it could be evaluated in
-- the REPL (remember: you’ll need let when entering it directly into
-- the REPL). Be sure to enter your code into the REPL to make sure it
-- evaluates correctly.
-- z = 7
-- x = y ^ 2
-- waxOn = x * 5
-- y = z + 8

-- R:
-- *LetWhereInterm> let z = 7
-- *LetWhereInterm> let y = z + 8
-- *LetWhereInterm> let x = y ^ 2
-- *LetWhereInterm> let waxOn = x * 5

-- 1. Now you have a value called waxOn in your REPL. What do you
-- think will happen if you enter:
-- 10 + waxOn    -- 1135
-- -- or
-- (+10) waxOn   -- 1135
-- -- or
-- (-) 15 waxOn  -- 15-1125 = - 1110
-- -- or
-- (-) waxOn 15  -- 1125-15 = 1110

-- 2. Earlier we looked at a function called triple. While your REPL
-- has waxOn in session, re-enter the triple function at the prompt:
-- let triple x = x * 3
-- 3. Now, what will happen if we enter this at our GHCi prompt. Try
-- to reason out what you think will happen first, considering what
-- role waxOn is playing in this function call. Then enter it, see what
-- does happen, and check your understanding:
-- triple waxOn
-- R: It triples the value in waxOn variable.

-- 4. Rewrite waxOn as a function with a where clause in your source
-- file. Load it into your REPL and make sure it still works as expected!
waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

triple x = x * 3

-- 5. Now to the same source file where you have waxOn, add the
-- triple function. Remember: You don’t need let and the function
-- name should be at the left margin (that is, not nested as one
-- of the waxOn expressions). Make sure it works by loading it into
-- your REPL and then entering triple waxOn again at the REPL
-- prompt. You should have the same answer as you did above.
-- R: 3375

-- 6. Now, without changing what you’ve done so far in that file, add
-- a new function called waxOff that looks like this:
-- waxOff x = triple x
waxOff x = triple x ^ 2

-- 7. Load the source file into your REPL and enter waxOff waxOn at
-- the prompt.
-- You now have a function, waxOff that can be applied to a variety
-- of arguments — not just waxOn but any (numeric) value you want
-- to put in for 𝑥. Play with that a bit. What is the result of waxOff
-- 10 or waxOff (-50)? Try modifying your waxOff function to do
-- something new — perhaps you want to first triple the 𝑥 value and
-- then square it or divide it by 10. Just spend some time getting
-- comfortable with modifying the source file code, reloading it,
-- and checking your modification in the REPL.