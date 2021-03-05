-- 1. A value of type [a] is c) a list whose elements are all of some type 𝑎
-- 2. A function of type [[a]] -> [a] could a) take a list of strings as an argument
-- 3. A function of type [a] -> Int -> a b) returns one element of type 𝑎 from a list
-- 4. A function of type (a, b) -> a c) takes a tuple argument and returns the first value

----------------------------------
-- Determine the type
----------------------------------
-- 1.

-- a) (* 9) 6
--    54 :: Num a=>a
-- b) head [(0,"doge"),(1,"kitteh")]
--    (0, "doge") :: Num a => (a,[Char])
-- c) head [(0 :: Integer ,"dostack ge"),(1,"kitteh")]
--    (0, "doge") :: (Integer, [Char])
-- d) if False then True else False
--    False :: Bool
-- e) length [1, 2, 3, 4, 5]
--    5 :: Int
-- f) (length [1, 2, 3, 4]) > (length "TACOCAT")
--    False :: Bool

-- 2. Given
-- x = 5
-- y = x + 5
-- w = y * 10
-- What is the type of w?
-- w :: Num a => a

-- 3. Given
-- x = 5
-- y = x + 5
-- z y = y * 10
-- What is the type of z?
-- z :: Num a => a -> a

-- 4. Given
-- x = 5
-- y = x + 5
-- f = 4 / y
-- What is the type of f?
-- f :: Fractional a => a

-- 5. Given
-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- f = x ++ y ++ z
-- What is the type of f?
-- f :: [Char]


----------------------------------
-- Does it compile ?
----------------------------------

-- 1. bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10
-- DOES NOT COMPILE

-- 2. x = print
-- y = print "woohoo!"
-- z = x "hello world"
-- COMPILES

-- 3. a = (+)
-- b = 5
-- c = b 10
-- d = c 200
-- DOES NOT COMPILE . b is not a function

-- 4. a = 12 + b
-- b = 10000 * c
-- DOES NOT COMPILE. c is not in scope

----------------------------------
-- Type variable or specific type constructor ?
----------------------------------


-- 1. You will be shown a type declaration, and you should categorize
-- each type. The choices are a fully polymorphic type variable,
-- constrained polymorphic type variable, or concrete type constructor.
-- f :: Num a => a -> b -> Int -> Int
-- --           [0]  [1]   [2]    [3]
-- Here, the answer would be: constrained polymorphic (Num)
-- ([0]), fully polymorphic ([1]), and concrete ([2] and [3]).


-- 2. Categorize each component of the type signature as described
-- in the previous example.
-- f :: zed -> Zed -> Blah
--      [0]    [1]     [2]
--  [0] -> fully polymorphic [1],[2] -> concrete

-- 3. Categorize each component of the type signature
-- f :: Enum b => a -> b -> C
--               [0]  [1]  [2]
--  [0] -> fully polymorphic [1] -> constrained polymorphic [2] -> concrete 

-- 4. Categorize each component of the type signature
-- f :: f -> g -> C
--     [0]  [1]  [2]
--  [0],[1] -> fully polymorphic [2] -> concrete


----------------------------------
-- Write a type signature
----------------------------------


-- For the following expressions, please add a type signature. You should
-- be able to rely on GHCi type inference to check your work, although
-- you might not have precisely the same answer as GHCi gives (due to
-- polymorphism, etc).

-- 1. While we haven’t fully explained this syntax yet, you’ve seen it
-- in Chapter 2 and as a solution to an exercise in Chapter 4. This
-- syntax is a way of destructuring a single element of a list.
-- functionH ::
-- functionH (x:_) = x

-- functionH :: [a] -> a   // head


-- 2. functionC ::
-- functionC x y = if (x > y) then True else False

-- functionC :: Ord a => a -> a -> Bool


-- 3. functionS ::
-- functionS (x, y) = y
-- functionS :: (a,b) -> b  // snd


----------------------------------
-- Given a type, write the function
----------------------------------


-- You will be shown a type and a function that needs to be written. Use
-- the information the type provides to determine what the function
-- should do. We’ll also tell you how many ways there are to write the
-- function. (Syntactically different but semantically equivalent implementations
-- are not counted as being different).



-- 1. There is only one implementation that typechecks.
-- i :: a -> a
-- i = undefined

i :: a -> a
i x = x


-- 2. There is only one version that works.
-- c :: a -> b -> a
-- c = undefined

c :: a -> b -> a
c x y = x


-- 3. Given alpha equivalence are c” and c (see above) the same thing?
-- c'' :: b -> a -> b
-- c'' = ?

c'' :: b -> a -> b
c'' y x = y
-- Yes. They are the same thing


-- 4. Only one version that works.
-- c' :: a -> b -> b
-- c' = undefined

c' :: a -> b -> b
c' x y = y


-- 5. There are multiple possibilities, at least two of which you’ve seen
-- in previous chapters.
-- r :: [a] -> [a]
-- r = undefined

r :: [a] -> [a]
r x = take 3 x

r' :: [a] -> [a]
r' x = drop 3 x


-- 6. Only one version that will typecheck.
-- co :: (b -> c) -> (a -> b) -> (a -> c)
-- co = undefined

-- co f g = \a -> f (g a)
-- co f g a = f (g a)
-- co f g a = (f . g) a
-- co f g = (f . g)
-- co f g = (.) f g
-- co = (.)
co :: (b -> c) -> (a -> b) -> (a -> c)
co = (.)


-- 7. One version will typecheck.
-- a :: (a -> c) -> a -> a
-- a = undefined

a :: (a -> c) -> a -> a
a _ x = x


-- 8. One version will typecheck.
-- a' :: (a -> b) -> a -> b
-- a' = undefined

a' :: (a -> b) -> a -> b
a' f x = f x


----------------------------------
-- Fix it
----------------------------------


-- 1. module sing where
-- fstString :: [Char] ++ [Char]
-- fstString x = x ++ " in the rain"
-- sndString :: [Char] -> Char
-- sndString x = x ++ " over the rainbow"
-- sing = if (x > y) then fstString x or sndString y
-- where x = "Singin"
-- x = "Somewhere"

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y)
    then fstString x
    else sndString y
    where x = "Singin"

y = "Somewhere"


-- 2. Now that it’s fixed, make a minor change and make it sing the
-- other song. If you’re lucky, you’ll end up with both songs stuck
-- in your head!

sing' = if (x < y)
    then fstString x
    else sndString y
    where x = "Singin"


-- 3. -- arith3broken.hs
-- module Arith3Broken where
-- main :: IO ()
-- Main = do
-- print 1 + 2
-- putStrLn 10
-- print (negate -1)
-- print ((+) 0 blah)
-- where blah = negate 1

-- module Arith3Broken where

main :: IO()

main = do
    print (1 + 2)
    putStrLn "10"
    print (negate (-1))
    print ((+) 0 blah)
        where blah = negate 1


----------------------------------
-- Type-Kwon-Do
----------------------------------

-- 0
data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
-- g = ???
g (b, w) = (b, f w)


--1
f1 :: Int -> String
f1 = undefined

g1 :: String -> Char
g1 = undefined

h1 :: Int -> Char
-- h1 = ???
h1 = g1 . f1


--2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
-- e = ???
e = w . q


--3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
-- xform = ???
xform (a, b) = (xz a, yz b)


--4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
-- munge = ???
munge f g x = fst $ (g . f) x