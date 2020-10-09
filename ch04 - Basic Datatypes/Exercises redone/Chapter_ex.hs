module Chapter_ex where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- length is a function that takes a list and returns a result that tells how
-- many items are in the list.
-- 1. Given the definition of length above, what would the type signature
-- be? How many arguments, of what type does it take? What
-- is the type of the result it evaluates to?
-- R: length :: [a] -> Integer


-- 2. What are the results of the following expressions?
-- a) length [1, 2, 3, 4, 5]            -- 5
-- b) length [(1, 2), (2, 3), (3, 4)]   -- 3
-- c) length allAwesome                 -- 2
-- d) length (concat allAwesome)        -- 5


-- 3. Given what we know about numeric types and the type signature
-- of length, look at these two expressions. One works and one
-- returns an error. Determine which will return an error and why.
-- (n.b., If you’re checking the type signature of length in GHC
-- 7.10, you will find Foldable t => t a representing [a], as with
-- concat in the previous chapter. Again, consider Foldable t to
-- represent a list here, even though list is only one of the possible
-- types. We will explain it in detail later.)
-- Prelude> 6 / 3
-- -- and
-- Prelude> 6 / length [1, 2, 3]
-- R: The second one will return an error . Int is not an instance of Fractional typeclass


-- 4. How can you fix the broken code from the preceding exercise
-- using a different division function/operator?
-- R: div 6 (length [1,2,3])


-- 5. What is the type of the expression 2 + 3 == 5? What would we
-- expect as a result?
-- R: Bool. The result is True


-- 6. What is the type and expected result value of the following:
-- Prelude> let x = 5
-- Prelude> x + 3 == 5


-- 7. Below are some bits of code. Which will work? Why or why not?
-- If they will work, what value would these reduce to?
-- Prelude> length allAwesome == 2
-- Prelude> length [1, 'a', 3, 'b']
-- Prelude> length allAwesome + length awesome
-- Prelude> (8 == 8) && ('b' < 'a')
-- Prelude> (8 == 8) && 9


-- 8. Write a function that tells you whether or not a given String (or
-- list) is a palindrome. Here you’ll want to use a function called
-- ’reverse,’ a predefined function that does just what it sounds like.
-- reverse :: [a] -> [a]
-- reverse "blah"
-- "halb"
-- isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome x = undefined


-- 9. Write a function to return the absolute value of a number using
-- if-then-else
-- myAbs :: Integer -> Integer
-- myAbs = undefined


-- 10. Fill in the definition of the following function, using fst and
-- snd:
-- f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f = undefined


------------------------
-- Reading syntax
------------------------
-- In the following examples, you’ll be shown syntactically incorrect
-- code. Type it in and try to correct it in your text editor, validating it
-- with GHC or GHCi.
-- 1. Here, we want a function that adds 1 to the length of a string
-- argument and returns that result.
-- x = (+)
-- F xs = w 'x' 1
-- where w = length xs


-- 2. This is supposed to be the identity function, id.
-- \ X = x


-- 3. When fixed, this function will return 1 from the value [1, 2, 3].
-- Hint: you may need to refer back to the section about variables
-- conventions in “Hello Haskell” to refresh your memory of this
-- notation.
-- \ x : xs -> x


-- 4. When fixed, this function will return 1 from the value (1, 2)
-- f (a b) = A


---------------------------------------------
-- Match the function names to their types
---------------------------------------------

-- 1. Which of the following types is the type of show?
-- a) show a => a -> String
-- b) Show a -> a -> String
-- c) Show a => a -> String


-- 2. Which of the following types is the type of (==)?
-- a) a -> a -> Bool
-- b) Eq a => a -> a -> Bool
-- c) Eq a -> a -> a -> Bool
-- d) Eq a => A -> Bool


-- 3. Which of the following types is the type of fst?
-- a) (a, b) -> a
-- b) b -> a
-- c) (a, b) -> b


-- 4. Which of the following types is the type of (+)?
-- a) Num a -> a -> a -> Bool
-- b) Num a => a -> a -> Bool
-- c) num a => a -> a -> a
-- d) Num a => a -> a -> a
-- e) a -> a -> a