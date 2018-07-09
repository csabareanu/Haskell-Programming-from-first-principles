module Chapter_ex where

------------------
--Multiple choice
------------------


-- 1. The Eq class
-- a) includes all types in Haskell
-- b) is the same as the Ord class
-- c) makes equality tests possible
-- d) only includes numeric types

-- c)


-- 2. The typeclass Ord
-- a) allows any two values to be compared
-- b) is a subclass of Eq
-- c) is a superclass of Eq
-- d) has no instance for Bool

-- b)


-- 3. Suppose the typeclass Ord has an operator > . What is the type
-- of > ?
-- a) Ord a => a -> a -> Bool
-- b) Ord a => Int -> Bool
-- c) Ord a => a -> Char
-- d) Ord a => Char -> [Char]

-- a)


-- 4. In x = divMod 16 12
-- a) the type of x is Integer
-- b) the value of x is undecidable
-- c) the type of x is a tuple
-- d) x is equal to 12 / 16

-- c)


-- 5. The typeclass Integral includes
-- a) Int and Integer numbers
-- b) integral, real, and fractional numbers
-- c) Schrodinger’s cat
-- d) only positive numbers

-- a)


-----------------------
-- Does it typecheck ?
-----------------------

--1. Does the following code typecheck? If not, why not?
-- data Person = Person Bool
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)


-- Person does not have an instance of Show. Solution:
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


--2. Does the following typecheck? If not, why not?
-- data Mood = Blah
--            | Woot deriving Show
-- settleDown x = if x == Woot
--                  then Blah
--                  else x

-- Mood does not have an instance of Eq. Solution:
data Mood = Blah
           | Woot deriving (Show, Eq)
settleDown x = if x == Woot
                 then Blah
                 else x


-- 3. If you were able to get settleDown to typecheck:
-- a) What values are acceptable inputs to that function?
-- Only values of type Mood

-- b) What will happen if you try to run settleDown 9 ? Why?
-- 9 is not a value of type Mood

-- c) What will happen if you try to run Blah > Woot ? Why?
-- Mood does not have an instance of type Ord. We could replace Eq with Ord in the Mood data type declaration because Ord implies Eq


-- 4. Does the following typecheck? If not, why not?
type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Yes, it typechecks but s1 cannot be printed on the screen because it is a function and not a Sentence data type


-------------------------------------------------
-- Given a datatype declaration, what can we do?
-------------------------------------------------

-- Given the following datatype definitions:
data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)
--Which of the following will typecheck? For the ones that don’t type-
--check, why don’t they?
--1.
-- phew = Papu "chases" True

-- It should be:
phew = Papu (Rocks "chases") (Yeah True)

--2.
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- It typechecks.


-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- It typechecks


--4.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

-- Papu does not have an instance of type Ord.


------------------
--Match the type
------------------

-- We’re going to give you two types and their implementations. Then
-- we’re going to ask you if you can substitute the second type for the
-- first. You can test this by typing the first declaration and its type into a file and editing in the new one, loading to see if it fails.
-- 1. For the following definition.
-- a)
-- i :: Num a => a
-- i = 1
-- b) Try replacing the type signature with the following:
-- i :: a
-- After you’ve formulated your own answer, then tested that
-- answer and believe you understand why you were right or
-- wrong, make sure to use GHCi to check what type GHC infers
-- for the definitions we provide without a type assigned. For
-- example, for this one, you’d type in:
-- Prelude> let i = 1
-- Prelude> :t i
-- Result elided intentionally.

-- Cannot substitute


-- 2.
-- a)
-- f :: Float
-- f = 1.0
-- b) f :: Num a => a

-- You cannot substitute the types because Num is to general


-- 3. a)
-- f :: Float
-- f = 1.0
-- b) f :: Fractional a => a


-- 4. Hint for the following: type :info RealFrac in your REPL
-- a)
-- f :: Float
-- f = 1.0
-- b) f :: RealFrac a => a


-- 5.
-- a)
-- freud :: a -> a
-- freud x = x
-- b) freud :: Ord a => a -> a



-- 6. a)
-- freud' :: a -> a
-- freud' x = x
-- b) freud' :: Int -> Int



-- 7. a)
-- myX = 1 :: Int
-- sigmund :: Int -> Int
-- sigmund x = myX
-- b) sigmund :: a -> a



-- 8.
-- a) myX = 1 :: Int
-- sigmund' :: Int -> Int
-- sigmund' x = myX
-- b) sigmund' :: Num a => a -> a


-- 9.
-- a) You’ll need to import sort from Data.List .
-- jung :: Ord a => [a] -> a
-- jung xs = head (sort xs)
-- b) jung :: [Int] -> Int


-- 10.
-- a) young :: [Char] -> Char
-- young xs = head (sort xs)
-- b) young :: Ord a => [a] -> a


-- 11.
-- a) mySort :: [Char] -> [Char]
-- mySort = sort
-- signifier :: [Char] -> Char
-- signifier xs = head (mySort xs)
-- b)
-- signifier :: Ord a => [a] -> a