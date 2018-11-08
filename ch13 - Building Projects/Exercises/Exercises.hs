module Exercises where

-- 2. Here is a very simple, short block of code. Notice it has a forever
-- that will make it keep running, over and over again. Load it into
-- your REPL and test it out. Then refer back to the chapter and
-- modify it to exit successfully after a False result.

import Control.Monad
import System.Exit(exitSuccess)
import Data.Char


palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True  -> putStrLn "It's a palindrome!"
        -- False -> putStrLn "Nope!"
        False -> exitSuccess

-- 3. If you tried using palindrome on a sentence such as “Madam I’m
-- Adam,” you may have noticed that palindrome checker doesn’t
-- work on that. Modifying the above so that it works on sentences,
-- too, involves several steps. You may need to refer back to previous
-- examples in the chapter to get ideas for proper ordering and
-- nesting. You may wish to import Data.Char to use the function
-- toLower. Have fun.

allLowerCase :: String -> String
allLowerCase = map (\a -> toLower a)

dropNonLetters :: String -> String
dropNonLetters = filter (\a -> elem a (['a'..'z'] ++ ['A'..'Z']))

palindromeBetter :: IO ()
palindromeBetter = forever $ do
    line1 <- getLine
    case ( isPalindrome line1) of
        True  -> putStrLn "It's a palindrome!"
        False -> exitSuccess
    where
        isPalindrome :: String -> Bool
        isPalindrome line = simplified == reverse simplified
            where simplified = allLowerCase . dropNonLetters $line


-- 4.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =  NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String
                    deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                        "Name was: " ++ show name ++
                                        " Age was: " ++ show age

-- Your job is to write the following function without modifying the
-- code above.
gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Enter name: "
    name <- getLine
    putStrLn "Enter age: "
    age <- readLn :: IO Integer
    return ()
    -- let person = mkPerson name age in
        -- case (person) of
        --     (Person _ _) -> putStrLn "Yay!!"
        --     otherwise    -> putStrLn "hha"
            --(PersonInvalid ) -> putStrLn "The following error occured: " ++


-- Since IO () is about the least informative type imaginable, we’ll
-- tell what it should do.
-- a) It should prompt the user for a name and age input.
-- b) It should attempt to construct a Person value using the name
-- and age the user entered. You’ll need the read function for
-- Age because it’s an Integer rather than a String.
-- c) If it constructed a successful person, it should print ”Yay!
-- Successfully got a person:” followed by the Person value.
-- d) If it got an error value, report that an error occurred and
-- print the error.