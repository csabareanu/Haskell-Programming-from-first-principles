-- 1. Binding or bound is a common word used to indicate connection,
-- linkage, or association between two objects. In Haskell we’ll use
-- it to talk about what value a variable has, e.g., a parameter variable
-- is bound to an argument value, meaning the value is passed
-- into the parameter as input and each occurrence of that named
-- parameter will have the same value. Bindings as a plurality will
-- usually refer to a collection of variables and functions which can
-- be referenced by name.
-- blah :: Int
-- blah = 10
-- Here the variable blah is bound to the value 10.


-- 2. An anonymous function is a function which is not bound to an identifier
-- and is instead passed as an argument to another function
-- and/or used to construct another function. See the following
-- examples.
-- \x -> x
-- -- anonymous version of id
-- id x = x
-- -- not anonymous, it's bound to 'id'


-- 3. Currying is the process of transforming a function that takes
-- multiple arguments into a series of functions which each take
-- one argument and one result. This is accomplished through the
-- nesting. In Haskell, all functions are curried by default. You
-- don’t need to do anything special yourself.
-- -- curry and uncurry already exist in Prelude
-- curry' :: ((a, b) -> c) -> a -> b -> c
-- curry' f a b = f (a, b)
-- uncurry' :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry' f (a, b) = f a b
-- -- uncurried function, takes a tuple of its arguments
-- add :: (Int, Int) -> Int
-- add (x, y) = x + y
-- add' :: Int -> Int -> Int
-- add' = curry' add
-- A function that appears to take two arguments is actually two
-- functions that each take one argument and return one result.
-- What makes this work is that a function can return another function.
-- f a b = a + b
-- -- is equivalent to
-- f = \a -> (\b -> a + b)


-- 4. Pattern matching is a syntactic way of deconstructing product and
-- sum types to get at their inhabitants. With respect to products,
-- pattern matching gives you the means for destructuring and
-- exposing the contents of products, binding one or more values
-- contained therein to names. With sums, pattern matching lets
-- you discriminate which inhabitant of a sum you mean to handle
-- in that match. It’s best to explain pattern matching in terms of
-- how datatypes work, so we’re going to use terminology that you
-- may not fully understand yet. We’ll cover this more deeply soon.

-- -- nullary data constructor, not a sum or product.
-- -- Just a single value.
-- data Blah = Blah
-- Pattern matching on Blah can only do one thing.
-- blahFunc :: Blah -> Bool
-- blahFunc Blah = True

-- data Identity a =
-- Identity a
-- deriving (Eq, Show)
-- Identity is a unary data constructor. Still not a product, only
-- contains one value.

-- -- when you pattern match on Identity
-- -- you can unpack and expose the 'a'
-- unpackIdentity :: Identity a -> a
-- unpackIdentity (Identity x) = x
-- -- But you can choose to ignore
-- -- the contents of Identity
-- ignoreIdentity :: Identity a -> Bool
-- ignoreIdentity (Identity _) = True

-- -- or ignore it completely since matching on
-- -- a non-sum data constructor changes nothing.
-- ignoreIdentity' :: Identity a -> Bool
-- ignoreIdentity' _ = True

-- data Product a b =
-- Product a b
-- deriving (Eq, Show)
-- Now we can choose to use none, one, or both of the values in the
-- product of 𝑎 and 𝑏:
-- productUnpackOnlyA :: Product a b -> a
-- productUnpackOnlyA (Product x _) = x
-- productUnpackOnlyB :: Product a b -> b
-- productUnpackOnlyB (Product _ y) = y
-- Or we can bind them both to a different name:
-- productUnpack :: Product a b -> (a, b)
-- productUnpack (Product x y) = (x, y)
-- What happens if you try to bind the values in the product to the
-- same name?
-- data SumOfThree a b c =
-- FirstPossible a
-- -- | SecondPossible b
-- -- | ThirdPossible c
-- deriving (Eq, Show)
-- Now we can discriminate by the inhabitants of the sum and
-- choose to do different things based on which constructor in the
-- sum they were.
-- sumToInt :: SumOfThree a b c -> Integer
-- sumToInt (FirstPossible _) = 0
-- sumToInt (SecondPossible _) = 1
-- sumToInt (ThirdPossible _) = 2
-- -- We can selectively ignore inhabitants of the sum
-- sumToInt :: SumOfThree a b c -> Integer
-- sumToInt (FirstPossible _) = 0
-- sumToInt _ = 1
-- -- We still need to handle every possible value
-- Pattern matching is about your data.


-- 5. Bottom is a non-value used to denote that the program cannot
-- return a value or result. The most elemental manifestation of
-- this is a program that loops infinitely. Other forms can involve
-- things like writing a function that doesn’t handle all of its inputs
-- and fails on a pattern match. The following are examples of
-- bottom:
-- -- If you apply this to any values,
-- -- it'll recurse indefinitely.
-- f x = f x
-- -- It'll a'splode if you pass a False value
-- dontDoThis :: Bool -> Int
-- dontDoThis True = 1
-- -- morally equivalent to
-- definitelyDontDoThis :: Bool -> Int
-- definitelyDontDoThis True = 1
-- definitelyDontDoThis False = error "oops"
-- -- don't use error. We'll show you a better way soon.
-- Bottom can be useful as a canary for signaling when code paths
-- are being evaluated. We usually do this to determine how lazy
-- a program is or isn’t. You’ll see a lot of this in our chapter on
-- non-strictness later on.


-- 6. Higher-order functions are functions which themselves take functions
-- as arguments or return functions as results. Due to currying,
-- technically any function that appears to take more than one
-- argument is higher order in Haskell.
-- Technically higher order
-- because of currying
-- Int -> Int -> Int
-- -- See? Returns another function
-- -- after applying the first argument
-- Int -> (Int -> Int)
-- -- The rest of the following examples are
-- -- types of higher order functions
-- (a -> b) -> a -> b
-- (a -> b) -> [a] -> [b]
-- (Int -> Bool) -> [Int] -> [Bool]
-- -- also higher order, this one
-- -- takes a function argument which itself
-- -- is higher order as well.
-- ((a -> b) -> c) -> [a] -> [c]


-- 7. Composition is the application of a function to the result of having
-- applied another function. The composition operator is a higherorder
-- function as it takes the functions it composes as arguments
-- and then returns a function of the composition:
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- -- is actually
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- -- or
-- (.) :: (b -> c) -> ((a -> b) -> (a -> c))
-- -- can be implemented as
-- comp :: (b -> c) -> ((a -> b) -> (a -> c))
-- comp f g x = f (g x)
-- The function g is applied to x, f is applied to the result of g x.


-- 8. Pointfree is programming tacitly, or without mentioning arguments
-- by name. This tends to look like “plumby” code where
-- you’re routing data around implicitly or leaving off unnecessary
-- arguments thanks to currying. The “point” referred to in the
-- term pointfree is an argument.

-- -- not pointfree
-- blah x = x
-- addAndDrop x y = x + 1
-- reverseMkTuple a b = (b, a)
-- reverseTuple (a, b) = (b, a)
-- wtf d = zipWith (+) (\ l -> (map d l) >>= \ h -> h)

-- -- pointfree versions of the above
-- blah = id
-- addAndDrop = const . (1 +)
-- reverseMkTuple = flip (,)
-- reverseTuple = uncurry (flip (,))
-- wtf = zipWith (+) . (join .) . map
-- To see more examples like this, check out the Haskell Wiki page
-- on Pointfree at https://wiki.haskell.org/Pointfree.
