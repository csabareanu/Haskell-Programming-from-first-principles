module General where

-- The types are a way of classifying, organizing and delimiting data to only the forms we want to process in our programs.

-- Definitions:
-- 1) A DATA DECLARATION is how datatypes are defined

-- the definition of Bool
data Bool = False | True
--     [1]  [2]  [3] [4]

-- 1. Type constructor for datatype Bool. This is the name of the type
-- and shows up in type signatures.
-- 2. Data constructor for the value False. These are the values that show up in the code
-- 3. Pipe | indicates logical disjunction, “or.” So, a Bool value is True
-- or False.
-- 4. Data constructor for the value True.