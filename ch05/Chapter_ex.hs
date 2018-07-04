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
-- c) head [(0 :: Integer ,"doge"),(1,"kitteh")]
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
--  [0] -> fully polymorphic [1],[2] -> concrete polymorphic

-- 3. Categorize each component of the type signature
-- f :: Enum b => a -> b -> C
--               [0]  [1]  [2]
--  [0] -> fully polymorphic [1] -> constrained polymorphic [2] -> concrete polymorphic

-- 4. Categorize each component of the type signature
-- f :: f -> g -> C
--     [0]  [1]  [2]
--  [0],[1] -> fully polymorphic [2] -> concrete polymorphic


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