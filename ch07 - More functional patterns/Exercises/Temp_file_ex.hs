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
mainPF6 = do
    print (roundTripPF6 (4 :: Int) :: Double)

roundTripPF6_2 :: (Show a, Integral a, Read b, Fractional b) => a -> b
roundTripPF6_2 = read . show
mainPF6_2 = do
    print (roundTripPF6_2 4)