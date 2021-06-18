module Jammin where

import Data.List


-- Here we’ve started working on datatypes to keep track of Julie’s homemade
-- jam output, with an Int value to represent how many jars she’s
-- canned:
data Fruit =
    Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show, Ord)

-- data JamJars =
--     Jam Fruit Int
--     deriving (Eq, Show)

-- 1. Let’s make a module for this. Name your module at the top of
-- the file:
-- module Jammin where
-- 2. Rewrite JamJars with record syntax.
data JamJars =
    Jam {
        fruit :: Fruit
        , nr  :: Int
    }
    deriving (Eq, Show, Ord)

-- 3. What is the cardinality of JamJars?
-- Fruit * Int
-- 4 * cardinality of Int

-- 4. Add Ord instances to your deriving clauses.
-- Added Ord instance above

-- 5. You can use the record field accessors in other functions as well.
-- To demonstrate this, work up some sample data that has a count
-- of the types and numbers of jars of jam in the rows in our pantry
-- (you can define more data than this if you like):
row1 = Jam Peach 12
row2 = Jam Apple 15
row3 = Jam Blackberry 4
row4 = Jam Plum 12
row5 = Jam Plum 11
row6 = Jam Peach 4
allJam = [row1, row2, row3, row4, row5, row6]
-- Now over that list of data, we can map the field accessor for the
-- Int value and see a list of the numbers for each row.
allJamCount :: [Int]
allJamCount = map nr allJam


-- 6. Write a function that will return the total number of jars of jam.
sumJars :: [JamJars] -> Int
sumJars = sum . map nr

sumJars' :: [JamJars] -> Int
sumJars' = foldr (\a b -> nr a + b) 0


-- 7. Write a function that will tell you which row has the most jars of
-- jam in it. It should return a result like this, though the fruit and
-- number will vary depending on how you defined your data:
-- *Jammin> mostRow
-- Jam {fruit = Apple, jars = 10}

mostRow :: [JamJars] -> JamJars
mostRow (x : xs) = foldr (\a b -> if (nr a > nr b) then a else b) x xs

-- 8. Under your module name, import the module called Data.List.
-- It includes some standard functions called sortBy and groupBy
-- that will allow us to organize our list of jams. Look at their type
-- signatures because there are some important differences between
-- them.
-- *Jammin> :t sortBy
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- *Jammin> :t groupBy
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]


-- 9. You’ll want to sort the list allJams by the first field in each record.
-- You may (or may not) want to use the following helper function
-- as part of that:
compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJamsName :: [JamJars]
sortedJamsName = sortBy compareKind allJam

sortedJamsQuantity :: [JamJars]
sortedJamsQuantity = sortBy (\(Jam _ k) (Jam _ k') -> compare k k') allJam


-- 10. Now take the sorting function and use groupBy to group the
-- jams by the type of fruit they are made from. You’ll later want
-- the ability to sum the sublists separately, so you’re looking for a
-- result that is a list of lists (again, the actual data in your list will
-- depend on how you defined it):
-- *Jammin> groupJam
-- [ [ Jam {fruit = Peach, jars = 5}
-- , Jam {fruit = Peach, jars = 3} ]
-- , [ Jam {fruit = Plum, jars = 8}
-- , Jam {fruit = Plum, jars = 4} ]
-- , [ Jam {fruit = Apple, jars = 10} ]
-- , [ Jam {fruit = Blackberry, jars = 7}
-- , Jam {fruit = Blackberry, jars = 4} ] ]

groupedJamsName :: [[JamJars]]
groupedJamsName = groupBy (\(Jam _ k) (Jam _ k') -> k == k') allJam

groupFruit :: JamJars -> JamJars -> Bool
groupFruit x y = fruit x == fruit y

groupFruits :: [[JamJars]]
groupFruits = groupBy groupFruit sortedJamsName
