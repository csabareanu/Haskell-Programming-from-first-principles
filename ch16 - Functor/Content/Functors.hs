module Functors where


replaceWithP :: a -> Char
replaceWithP = const 'p'


-- *Functors> replaceWithP 1000
-- 'p'
-- *Functors> replaceWithP (Just 10)
-- 'p'
-- *Functors> replaceWithP Nothing
-- 'p'

-------------------------------------
-- data Maybe a = Nothing | Just a
-------------------------------------

-- *Functors> fmap replaceWithP (Just 10)
-- Just 'p'
-- *Functors> fmap replaceWithP Nothing
-- Nothing

-------------------------------
-- data [] a = [] | a : [a]
-------------------------------

-- *Functors> fmap replaceWithP [1,2,3,4,5]
-- "ppppp"
-- *Functors> fmap replaceWithP "Ave"
-- "ppp"
-- *Functors> fmap (+1) []
-- []
-- *Functors> fmap replaceWithP []
-- ""

----------------------------
-- data (,) a b = (,) a b
----------------------------
-- *Functors> fmap replaceWithP (10,20)
-- (10,'p')
-- *Functors> fmap replaceWithP (10, "woo")
-- (10,'p')

--------------------------------------------------------
-- the functor of functions (composition of functions)
--------------------------------------------------------
-- Prelude> let tossEmOne = fmap (+1) negate
-- Prelude> tossEmOne 10
-- -9
-- Prelude> tossEmOne (-10)
-- 11

-- Prelude> let tossEmOne' = (+1) . negate
-- Prelude> tossEmOne' 10
-- -9
-- Prelude> tossEmOne' (-10)
-- 11

--------------------------------------------------------------

-- *Functors> let lms = [Just "Ave", Nothing, Just "wohooo"]
-- *Functors> let replaceWithP = const 'p'
-- *Functors> replaceWithP lms
-- 'p'
-- Explanation: It doesn't matter what lms is . It will ALWAYS return 'p'

-- *Functors> fmap replaceWithP lms
-- "ppp"
-- Explanation: By applying fmap once we are going to leave the list structure unchanged around our result. ("ppp" :: [Char])


-- *Functors> (fmap . fmap) replaceWithP lms
-- [Just 'p',Nothing,Just 'p']
-- Explanation: By applying fmap twice we are going to leave 2 levels of structure untouched : the list and the Maybe
-- In : [Maybe String] , Out : [Maybe Char]


-- *Functors> (fmap . fmap . fmap) replaceWithP lms
-- [Just "ppp",Nothing,Just "pppppp"]
-- *Functors> :t (fmap . fmap . fmap) replaceWithP lms
-- (fmap . fmap . fmap) replaceWithP lms :: [Maybe [Char]]
-- Explanation: By applying fmap three times we are going to leave 3 levels of structure untouched : the list, the Maybe and the String(list of chars)
--              We lifted over the [] of [Char]

----------------------------------------------------------------------------------------------

-- *Functors> :t fmap replaceWithP lms
-- fmap replaceWithP lms :: [Char]

-- *Functors> :t (fmap . fmap) replaceWithP lms
-- (fmap . fmap) replaceWithP lms :: [Maybe Char]

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--          fmap        fmap
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> f x -> f y


-- replaceWithP' :: [Maybe [Char]] -> Char
-- replaceWithP' = replaceWithP
-- [Maybe [Char]] -> [Char]
-- [Maybe [Char]] -> [Maybe Char]
-- [Maybe [Char]] -> [Maybe [Char]]

-- The type of the last level of lifting is the same as the input type.