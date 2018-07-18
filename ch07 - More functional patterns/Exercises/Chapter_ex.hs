module Chapter_ex where

---------------------
-- Multiple choice
---------------------


-- 1. A polymorphic function
-- a) changes things into sheep when invoked
-- b) has multiple arguments
-- c) has a concrete type
-- d) may resolve to values of different types, depending on inputs

-- d)


-- 2. Two functions named f and g have types Char -> String and
-- String -> [String] respectively. The composed function g . f has the type
-- a) Char -> String
-- b) Char -> [String]
-- c) [[String]]
-- d) Char -> String -> [String]

-- b)


-- 3. A function f has the type Ord a => a -> a -> Bool and we
-- apply it to one numeric value. What is the type now?
-- a) Ord a => a -> Bool
-- b) Num -> Num -> Bool
-- c) Ord a => a -> a -> Integer
-- d) (Ord a, Num a) => a -> Bool

-- a)


-- 4. A function with the type (a -> b) -> c
-- a) requires values of three different types
-- b) is a higher-order function
-- c) must take a tuple as its first argument
-- d) has its parameters in alphabetical order

-- b)


-- 5. Given the following definition of f , what is the type of f True
-- f :: a -> a
-- f x = x
-- a) f True :: Bool
-- b) f True :: String
-- c) f True :: Bool -> Bool
-- d) f True :: a

-- a)


---------------------
-- Let’s write code
---------------------

-- 1. The following function returns the tens digit of an integral argument.
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

-- a) First, rewrite it using divMod .

tensDigit_a x = d
    where (xLast, _) = x `divMod` 10
          (_, d)     = xLast `divMod` 10

tensDigit_a2 x = snd . (`divMod` 10) . fst . (`divMod` 10) $ x


-- b) Does the divMod version have the same type as the original
-- version?

-- Yes, it does.


-- c) Next, let’s change it so that we’re getting the hundreds digit
-- instead. You could start it like this (though that may not be
-- the only possibility):
-- hunsD x = d2
--     where d = undefined
--     ...

hunsD :: Integral a => a -> a
hunsD = tensDigit . (`div` 10)


-- 2. Implement the function of the type a -> a -> Bool -> a
-- once each using a case expression and once with a guard.
-- foldBool :: a -> a -> Bool -> a
-- foldBool = error "Error: Need to implement foldBool!"
-- The result is semantically similar to if-then-else expressions
-- but syntactically quite different. Here is the pattern matching
-- version to get you started:

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

-- case expression
foldBool3_case :: a -> a -> Bool -> a
foldBool3_case x y z =
    case z of
        True -> x
        False -> y

-- guard
foldBool3_guard :: a -> a -> Bool -> a
foldBool3_guard x y z
        | z         = x
        | otherwise = y

-- 3. Fill in the definition. Note that the first argument to our function
-- is also a function which can be applied to values. Your second
-- argument is a tuple, which can be used for pattern matching:
g :: (a -> b) -> (a, c) -> (b, c)
-- g = undefined
g f (a, b) = (f a, b)