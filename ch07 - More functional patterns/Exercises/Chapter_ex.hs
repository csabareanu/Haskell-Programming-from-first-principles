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

-- d)


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

tensDigit_a :: Integral a => a -> a
tensDigit_a x = d
    where (xLast, _) = x `divMod` 10
          (_, d)     = xLast `divMod` 10

tensDigit_a2 :: Integral a => a -> a
tensDigit_a2 x = snd . (`divMod` 10) . fst . (`divMod` 10) $ x

tensDigit_b :: Integral a => a -> a
tensDigit_b = (`mod` 10) . (`div` 10)


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


-- 4. For this next exercise, you’ll experiment with writing pointfree
-- versions of existing code. This involves some new information,
-- so read the following explanation carefully.
-- Typeclasses are dispatched by type. Read is a typeclass like Show,
-- but it is the dual or “opposite” of Show. In general, the Read
-- typeclass isn’t something you should plan to use a lot, but this exercise
-- is structured to teach you something about the interaction
-- between typeclasses and types.
-- The function read in the Read typeclass has the type:
-- read :: Read a => String -> a
-- Notice a pattern?
-- read :: Read a => String -> a
-- show :: Show a => a -> String
-- Write the following code into a source file. Then load it and
-- run it in GHCi to make sure you understand why the evaluation
-- results in the answers you see.
-- arith4.hs
-- module Arith4 where
-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a) -- this is really similar to id a but a must have an instance of Show.
main = do
    print (roundTrip 4)
    print (id 4)


-- 5. Next, write a pointfree version of roundTrip.
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show
mainPF = do
    print (roundTripPF 4)
    print (id 4)


-- 6. We will continue to use the code in module Arith4 for this exercise as well.
-- When we apply show to a value such as (1 :: Int), the 𝑎 that
-- implements Show is Int, so GHC will use the Int instance of the
-- Show typeclass to stringify our Int of 1.
-- However, read expects a String argument in order to return an
-- 𝑎. The String argument that is the first argument to read tells
-- the function nothing about what type the de-stringified result
-- should be. In the type signature roundTrip currently has, it
-- knows because the type variables are the same, so the type that is
-- the input to show has to be the same type as the output of read.
-- Your task now is to change the type of roundTrip in Arith4 to
-- (Show a, Read b) => a -> b. How might we tell GHC which
-- instance of Read to dispatch against the String now? Make the
-- application of your pointfree version of roundTrip to the argument
-- 4 on line 10 work. You will only need the has the type syntax
-- of :: and parentheses for scoping.

roundTripPF6 :: (Show a, Read b) => a -> b
roundTripPF6 = read . show

mainPF6 :: IO ()
mainPF6 = do
    print (roundTripPF6 (4 :: Int) :: Double)


-- Forcing b to be of different type than a from the type signature.
roundTripPF6_2 :: (Show a, Integral a, Read b, Fractional b) => a -> b
roundTripPF6_2 = read . show

mainPF6_2 :: IO ()
mainPF6_2 = do
    print (roundTripPF6_2 4)