module Chapter_ex where

import Data.Char

-------------------
-- Multiple choice
-------------------

-- 1. Given the following datatype:
data Weekday =
    Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
-- we can say:
-- a) Weekday is a type with five data constructors
-- b) Weekday is a tree with five branches
-- c) Weekday is a product type
-- d) Weekday takes five arguments

-- a)


-- 2. and with the same datatype definition in mind, what is the type
-- of the following function, f?
f Friday = "Miller Time"
-- a) f :: [Char]
-- b) f :: String -> String
-- c) f :: Weekday -> String
-- d) f :: Day -> Beer

-- c)


-- 3. Types defined with the data keyword
-- a) must have at least one argument
-- b) must begin with a capital letter
-- c) must be polymorphic
-- d) cannot be imported from modules

-- b)


-- 4. The function g xs = xs !! (length xs - 1)
-- a) is recursive and may not terminate
-- b) delivers the head of xs
-- c) delivers the final element of xs
-- d) has the same type as xs

-- c)


------------
-- Ciphers
------------


-- In the Lists chapter, you wrote a Caesar cipher. Now, we want to
-- expand on that idea by writing a Vigenère cipher. A Vigenère cipher
-- is another substitution cipher, based on a Caesar cipher, but it
-- uses a series of Caesar ciphers for polyalphabetic substitution. The
-- substitution for each letter in the plaintext is determined by a fixed
-- keyword.
-- So, for example, if you want to encode the message “meet at dawn,”
-- the first step is to pick a keyword that will determine which Caesar
-- cipher to use. We’ll use the keyword “ALLY” here. You repeat the
-- keyword for as many characters as there are in your original message:
-- MEET AT DAWN
-- ALLY AL LYAL
-- Now the number of rightward shifts to make to encode each character
-- is set by the character of the keyword that lines up with it. The ’A’
-- means a shift of 0, so the initial M will remain M. But the ’L’ for our
-- second character sets a rightward shift of 11, so ’E’ becomes ’P’. And
-- so on, so “meet at dawn” encoded with the keyword “ALLY” becomes
-- “MPPR AE OYWY.”
-- Like the Caesar cipher, you can find all kinds of resources to help you
-- understand the cipher and also many examples written in Haskell.
-- Consider using a combination of chr, ord, and mod again, possibly
-- very similar to what you used for writing the original Caesar cipher.

encode :: Char -> Int
encode x = ord x - ord 'a'

decode :: Int -> Char
decode x = chr (x + ord 'a')

shiftRight :: Int -> Char -> Char
shiftRight = shift (+)

shiftLeft :: Int -> Char -> Char
shiftLeft = shift (-)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f x y = decode $ mod (f (encode y) x) 26

charsPerWord :: [Char] -> [Int]
charsPerWord = map length . words

mapKeyword :: [Int] -> [Char] -> [Char]
mapKeyword x k =
    mapK []     _  = []
    mapK (x:xs) kw = take x ks : mapK xs (drop (x+1) ks)
    where
        ks = cycle kw





-- vigenere :: [Char] -> [Char] -> [Char]
-- vigenere k =
--     map (shiftRight )

keyword = "ally"
message = "meet me at dawn"

encodedMessage = "mppr ae oywy"

-- testCypher =
--     if vigenere message == encodedMessage
--         then print "Coorect"
--         else error "Not Correct"

----------------
-- As-patterns
----------------



-- “As-patterns” in Haskell are a nifty way to be able to pattern match on
-- part of something and still refer to the entire original value. Some
-- examples:
-- f :: Show a => (a, b) -> IO (a, b)
-- f t@(a, _) = do
-- print a
-- return t
-- Here we pattern-matched on a tuple so we could get at the first value
-- for printing, but used the @ symbol to introduce a binding named t
-- in order to refer to the whole tuple rather than just a part.
-- Prelude> f (1, 2)
-- 1
-- (1,2)
-- We can use as-patterns with pattern matching on arbitrary data constructors,
-- which includes lists:
-- doubleUp :: [a] -> [a]
-- doubleUp [] = []
-- doubleUp xs@(x:_) = x : xs
-- Prelude> doubleUp []
-- []
-- Prelude> doubleUp [1]
-- [1,1]
-- Prelude> doubleUp [1, 2]
-- [1,1,2]
-- Prelude> doubleUp [1, 2, 3]
-- [1,1,2,3]
-- Use as-patterns in implementing the following functions:
-- 1. This should return True if (and only if) all the values in the first
-- list appear in the second list, though they need not be contiguous.
-- isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
-- The following are examples of how this function should work:
-- Prelude> isSubsequenceOf "blah" "blahwoot"
-- True
-- Prelude> isSubsequenceOf "blah" "wootblah"
-- True
-- Prelude> isSubsequenceOf "blah” "wboloath"
-- True
-- Prelude> isSubsequenceOf "blah" "wootbla"
-- False
-- 2. Split a sentence into words, then tuple each word with the capitalized
-- form of each.
-- capitalizeWords :: String -> [(String, String)]
-- Prelude> capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]
