module Chapter_ex where

-- awesome = ["Papuchon", "curry", ":)"]
-- alsoAwesome = ["Quake", "The Simons"]
-- allAwesome = [awesome, alsoAwesome]

-- length is a function that takes a list and returns a result that tells how
-- many items are in the list.

-- 1. Given the definition of length above, what would the type signature
-- be? How many arguments, of what type does it take? What
-- is the type of the result it evaluates to?

-- length :: Foldable t => t a -> Int


-- 2. What are the results of the following expressions?
-- a) length [1, 2, 3, 4, 5]
-- b) length [(1, 2), (2, 3), (3, 4)]
-- c) length allAwesome
-- d) length (concat allAwesome)

-- a) 5
-- b) 3
-- c) 2
-- d) 5


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

-- Prelude> 6 / length [1, 2, 3] will return an error because length is of type Int and Int is not an instance of Fractional typeclass.


-- 4. How can you fix the broken code from the preceding exercise
-- using a different division function/operator?

-- div 6 (length [1,2,3])


-- 5. What is the type of the expression 2 + 3 == 5? What would we
-- expect as a result?

-- The type is Bool and the result is True


-- 6. What is the type and expected result value of the following:
-- Prelude> let x = 5
-- Prelude> x + 3 == 5

-- The type is Bool and the result is False


-- 7. Below are some bits of code. Which will work? Why or why not?
-- If they will work, what value would these reduce to?

-- Prelude> length allAwesome == 2
-- Works !!! // True

-- Prelude> length [1, 'a', 3, 'b']
-- Does not work. A list must have all the elements of the same type.

-- Prelude> length allAwesome + length awesome
-- Works !!! // 5

-- Prelude> (8 == 8) && ('b' < 'a')
-- Works !!! // True && False = False

-- Prelude> (8 == 8) && 9
-- Does not work. The second argument is not Bool.



-- 8. Write a unction that tells you whether or not a given String (or
-- list) is a palindrome. Here you’ll want to use a function called
-- ’reverse,’ a predefined function that does just what it sounds like.
-- reverse :: [a] -> [a]
-- reverse "blah"
-- "halb"
-- isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome x = undefined

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x


-- 9. Write a function to return the absolute value of a number using
-- if-then-else
-- myAbs :: Integer -> Integer
-- myAbs = undefined

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- 10. Fill in the definition of the following function, using fst and
-- snd:
-- f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f = undefined

f :: (a,b) -> (c,d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))


---------------------
--Reading Syntax
---------------------
-- In the following examples, you’ll be shown syntactically incorrect
-- code. Type it in and try to correct it in your text editor, validating it
-- with GHC or GHCi.


-- 1. Here, we want a function that adds 1 to the length of a string
-- argument and returns that result.
-- x = (+)
-- F xs = w 'x' 1
-- where w = length xs

x = (+)
g xs = x w 1
    where w = length xs


-- 2. This is supposed to be the identity function, id.
-- \ X = x

h x = x

-- 3. When fixed, this function will return 1 from the value [1, 2, 3].
-- Hint: you may need to refer back to the section about variables
-- conventions in “Hello Haskell” to refresh your memory of this
-- notation.
-- \ x : xs -> x

z (x:xs) = x 

-- 4. When fixed, this function will return 1 from the value (1, 2)
-- f (a b) = A

y (a,b) = a

1. Which of the following types is the type of show?
a) show a => a -> String
b) Show a -> a -> String
c) Show a => a -> String

c)

2. Which of the following types is the type of (==)?
a) a -> a -> Bool
b) Eq a => a -> a -> Bool
c) Eq a -> a -> a -> Bool
d) Eq a => A -> Bool

b)

3. Which of the following types is the type of fst?
a) (a, b) -> a
b) b -> a
c) (a, b) -> b

a)

4. Which of the following types is the type of (+)?
a) Num a -> a -> a -> Bool
b) Num a => a -> a -> Bool
c) num a => a -> a -> a
d) Num a => a -> a -> a
e) a -> a -> a

d)