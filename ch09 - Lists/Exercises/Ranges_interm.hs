module Ranges_interm where

-- ranges have syntactic sugar in them:
-- Prelude> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- Prelude> enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10]

-- Prelude> [1,2..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- Prelude> enumFromThenTo 1 2 10
-- [1,2,3,4,5,6,7,8,9,10]


-------------
-- Exercise
-------------

-- Some things you’ll want to know about the Enum typeclass:
-- Prelude> :info Enum
-- class Enum a where
-- succ :: a -> a
-- pred :: a -> a
-- toEnum :: Int -> a
-- fromEnum :: a -> Int
-- enumFrom :: a -> [a]
-- enumFromThen :: a -> a -> [a]
-- enumFromTo :: a -> a -> [a]
-- enumFromThenTo :: a -> a -> a -> [a]
-- Prelude> succ 0
-- 1
-- Prelude> succ 1
-- 2
-- Prelude> succ 'a'
-- 'b'
-- Write your own enumFromTo definitions for the types provided. Do
-- not use range syntax to do so. It should return the same results as if
-- you did [start..stop].

eftBool :: Bool -> Bool -> [Bool]
-- eftBool = undefined
eftBool s e
    | s > e  = []
    | s == e    = [e]
    | otherwise = s : eftBool (succ s) e

eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd = undefined
eftOrd s e
    | s > e  = []
    | s == e    = [e]
    | otherwise = s : eftOrd (succ s) e


eftInt :: Int -> Int -> [Int]
-- eftInt = undefined
eftInt s e
    | s > e  = []
    | s == e    = [e]
    | otherwise = s : eftInt (succ s) e


eftChar :: Char -> Char -> [Char]
-- eftChar = undefined
eftChar s e
    | s > e  = []
    | s == e    = [e]
    | otherwise = s : eftChar (succ s) e

-- abstraction
eftGen :: (Enum a, Ord a) => a -> a -> [a]
eftGen s e
    | s > e  = []
    | s == e    = [e]
    | otherwise = s : eftGen (succ s) e