module Data_decl_interm where

-- Given the following datatype, answer the following questions:
-- data Mood = Blah | Woot deriving Show

-- 1. What is the type constructor, or name of this type?

-- Mood


-- 2. If the function requires a Mood value, what are the values you
-- could possibly use there?

-- Blah or Woot


-- 3. We are trying to write a function changeMood to change Chris’s
-- mood instantaneously. So far, we’ve written a type signature
-- changeMood :: Mood -> Woot. What’s wrong with that?

-- Woot is a data constructor. On the type level, type constructors must be used.
-- changeMood :: Mood -> Mood


-- 4. Now we want to write the function that changes his mood. Given
-- an input mood, it gives us the other one. Fix any mistakes and
-- complete the function:
-- changeMood Mood = Woot
-- changeMood _ = Blah

-- changeMood Blah = Woot
-- changeMood Woot = Blah


-- 5. Enter all of the above — datatype (including the “deriving Show”
-- bit), your corrected type signature, and the corrected function
-- into a source file. Load it and run it in GHCi to make sure you
-- got it right.

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah