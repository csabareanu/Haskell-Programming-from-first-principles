module Chapter_ex where

import Data.Char
import Data.List

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
-- of the following function, ff?
ff Friday = "Miller Time"
-- a) ff :: [Char]
-- b) ff :: String -> String
-- c) ff :: Weekday -> String
-- d) ff :: Day -> Beer

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

mapKeyword :: [Char] -> [Int] -> [Char]
mapKeyword _  []     = []
mapKeyword ks (x:xs) = take x ks ++ " " ++ mapKeyword (drop x ks) xs

vigenere :: [Char] -> [Char] -> [Char]
vigenere k m =
    zipWith (\a b -> if a /= ' ' then (shiftRight (encode a) b) else ' ')
    (mapKeyword (cycle k) . charsPerWord $m)
    m

keyword = "ally"
message = "meet at dawn"

encodedMessage = "mppr ae oywy"

testCypher =
    if vigenere keyword message == encodedMessage
        then print "Coorect"
        else error "Not Correct"

----------------
-- As-patterns
----------------



-- “As-patterns” in Haskell are a nifty way to be able to pattern match on
-- part of something and still refer to the entire original value. Some
-- examples:
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t
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
-- list appear in the second list, though they need not be contiguous

-- double foldr. Not using x,xs,l,ls, only t and u
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf t@(x:xs) u@(l:ls) =
    foldr (\c d -> if (foldr (\a b -> if c == a then True else b)
                             False
                             u)
                             then d else False)
          True
          t


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' (x:xs) l = elem x l && isSubsequenceOf' xs l

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
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words
    where capitalize []       = ([], [])
          capitalize s@(x:xs) = (s, toUpper x:xs)

-- Prelude> capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]


-----------------------
-- Language exercises
-----------------------


-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs


-- Example output.
-- Prelude> capitalizeWord "Titter"
-- "Titter"
-- Prelude> capitalizeWord "titter"
-- "Titter"


-- 2. Write a function that capitalizes sentences in a paragraph. Recognize
-- when a new sentence has begun by checking for periods.
-- Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph = newSentence True
    where
        newSentence _   ""     = ""
        newSentence new (x:xs)
            | x == '.'           = x : newSentence True xs
            | new && isLetter x  = toUpper x : newSentence False xs
            | otherwise          = x : newSentence new xs


-- Example result you should get from your function:
-- Prelude> capitalizeParagraph "blah. woot ha."
-- "Blah. Woot ha."



-------------------
-- Phone exercise
-------------------


-- This exercise by geophf originally for 1HaskellADay. Thank you for
-- letting us use this exercise!
-- Remember old-fashioned phone inputs for writing text where you
-- had to press a button multiple times to get different letters to come
-- up? You may still have to do this when you try to search for a movie
-- to watch using your television remote control. You’re going to write
-- code to translate sequences of button presses into strings and vice
-- versa.
-- So! Here is the layout of the phone:
---- -----------------------------------------
---- | 1 | 2 ABC | 3 DEF |
---- _________________________________________
---- | 4 GHI | 5 JKL | 6 MNO |
---- -------------------------------------------
---- | 7 PQRS | 8 TUV | 9 WXYZ |
---- -----------------------------------------
---- | * ^ | 0 + _ | # ., |
-- -----------------------------------------
-- Where star (*) gives you capitalization of the letter you’re writing to
-- your friends, and 0 is your space bar. To represent the digit itself, you
-- press that digit once more than the letters it represents. If you press
-- a button one more than than is required to type the digit, it wraps
-- around to the first letter. For example,
-- 2 -> 'A'
-- 22 -> 'B'
-- 222 -> 'C'
-- 2222 -> '2'
-- 22222 -> 'A'
-- So on and so forth. We’re going to kick this around.
-- 1. Create a data structure that captures the phone layout above.
-- The data structure should be able to express enough of how the
-- layout works that you can use it to dictate the behavior of the
-- functions in the following exercises.
-- -- fill in the rest.
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [(Digit, [Digit])]

phone :: DaPhone
phone = DaPhone [('1', "1")
                 , ('2', "abc2")
                 , ('3', "def3")
                 , ('4', "ghi4")
                 , ('5', "jkl5")
                 , ('6', "mno6")
                 , ('7', "pqrs7")
                 , ('8', "tuv8")
                 , ('9', "wxyz9")
                 , ('*', "^*")
                 , ('0', "+_0")
                 , ('#', ".,#")
                ]

-- 2. Convert the following conversations into the keypresses required
-- to express them. We’re going to suggest types and functions to
-- fill in order to accomplish the goal, but they’re not obligatory. If
-- you want to do it differently…you do you.
convo :: [String] --[Char] = String
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone p) s
    | isUpper s  =  [('*', 1), presses p]
    | otherwise  =  [presses p]
    where
        presses []                      = ('#', 2)
        presses (x:xs)
            | s == ' '                  = ('0', 1)
            | elem  (toLower s) (snd x) = (fst x, indexOf (toLower s) (snd x) 1)
            | otherwise                 = presses xs

        indexOf t (y:ys) i
            | t == y    = i
            | otherwise = indexOf t ys (i+1)

-- -- assuming the default phone definition
-- -- 'a' -> ('2', 1)
-- -- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = foldr (\a b -> reverseTaps p a ++ b) []

mess :: [(Digit, Presses)]
mess = cellPhonesDead phone (convo !! 0)


-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> snd a + b) 0


-- 4. What was the most popular letter for each message? What was
-- its cost? You’ll want to combine reverseTaps and fingerTaps
-- to figure out what it cost in taps. reverseTaps is a list because
-- you need to press a different button in order to get capitals.

-- does not count white space and other punctuation (not all) and is case insensitive
mostPopularLetter :: String -> Char
mostPopularLetter s = foldr (\a b -> if (countLetters a s > countLetters b s) then a else b)
                      (head s)
                      (allLowerCase . removePunctuation . removeDuplicates $s)
        where
            countLetters :: Char -> String -> Int
            countLetters c = foldr (\a b -> if toLower a == c then b + 1 else b) 0

            removeDuplicates :: String -> String
            removeDuplicates = foldr (\a b -> if (elem a b) then b else a:b) ""

            removePunctuation :: String -> String
            removePunctuation [] = ""
            removePunctuation (x:xs) = if(elem x " .,?!;") then removePunctuation xs else x:removePunctuation xs

            allLowerCase :: String -> String
            allLowerCase [] = ""
            allLowerCase (x:xs) = toLower x : allLowerCase xs


costLetter :: Char -> Presses
costLetter = fingerTaps . reverseTaps phone

-- 5. What was the most popular letter overall? What was the most
-- popular word?

coolestLtr :: [String] -> Char
coolestLtr sl = mostPopularLetter . unwords $ sl


coolestWord :: [String] -> String
coolestWord s = foldr (\a b -> if (countWords a wordList > countWords b wordList) then a else b)
                      (head wordList)
                      (removeDuplicates wordList)
            where
                countWords :: String -> [String] -> Int
                countWords c = foldr (\a b -> if a == c then b + 1 else b) 0

                removeDuplicates :: [String] -> [String]
                removeDuplicates = foldr (\a b -> if (elem a b) then b else a:b) []

                removePunctuation :: String -> String
                removePunctuation [] = ""
                removePunctuation (x:xs) = if(elem x ".,?!;") then ' ':removePunctuation xs else x:removePunctuation xs

                allLowerCase :: String -> String
                allLowerCase [] = ""
                allLowerCase (x:xs) = toLower x : allLowerCase xs

                wordList :: [String]
                wordList = words . allLowerCase . removePunctuation . unwords $ s


------------------
-- Hutton’s Razor
------------------


-- Hutton’s Razor10 is a very simple expression language that expresses
-- integer literals and addition of values in that expression language.
-- The “trick” to it is that it’s recursive and the two expressions you’re
-- summing together could be literals or themselves further addition
-- operations. This sort of datatype is stereotypical of expression languages
-- used to motivate ideas in research papers and functional pearls.
-- Evaluating or folding a datatype is also in some sense what you’re
-- doing most of the time while programming anyway.
-- 1. Your first task is to write the “eval” function which reduces an
-- expression to a final sum.
-- data Expr
-- = Lit Integer
-- | Add Expr Expr
-- eval :: Expr -> Integer
-- eval = error "do it to it"
-- Example of expected output:
-- Prelude> eval (Add (Lit 1) (Lit 9001))
-- 9002
-- 2. Write a printer for the expressions.
-- printExpr :: Expr -> String
-- printExpr = undefined
-- Expected output:
-- Prelude> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
-- Prelude> let a1 = Add (Lit 9001) (Lit 1)
-- Prelude> let a2 = Add a1 (Lit 20001)
-- Prelude> let a3 = Add (Lit 1) a2
-- Prelude> printExpr a3
-- "1 + 9001 + 1 + 20001"
