module Currying_examples where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

--binding variables to types
funcIgnoreArgs :: a -> a -> a -> String
funcIgnoreArgs x y z = "Blah"

-- mapping a bool value to an integer
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

--Multiple parameters. First, the function applies the first argument(of type Integer) which returns a function that takes a Bool and returns an Integer
--typicalCurriedFunction :: Integer -> (Bool -> Integer) // (->) is a type constructor and right associative.
typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + nonsense b

--Only one parameter. Only one function application on the tuple
uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i,b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

-- de-sugar automatic currying
anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)


kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

-- *Currying_examples> :t kessel 1 2
-- kessel 1 2 :: (Ord a, Num a) => a