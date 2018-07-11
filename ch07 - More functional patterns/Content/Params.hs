module Params where

myNum :: Integer
myNum = 1

myVal = myNum
-- *Params> :t myVal
-- myVal :: Integer

myVal2 f = myNum
-- *Params> :t myVal2
-- myVal2 :: p -> Integer

myVal3 f = f + myNum
-- *Params> :t myVal3
-- myVal3 :: Integer -> Integer
-- f has to be of type Integer because we added it to myNum which is of type Integer


myNum2 = 2 -- it is no function bc no params are named between the name and the = sign.
-- *Params> :t myNum2
-- myNum2 :: Integer

myVal4 f = f + myNum2 -- f name for a param to the function myVal4
-- *Params> :t myVal4
-- myVal4 :: Integer -> Integer

stillAFunction a b c = a ++ b ++ c -- a, b, c args to the function stillAFunction
-- *Params> :t stillAFunction
-- stillAFunction :: [a] -> [a] -> [a] -> [a]