module Folds_interm2 where

--Write the following functions for processing this data.
import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
    [
    DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    , DbNumber 19001
    , DbNumber 1999
    ]
-- 1. Write a function that filters for DbDate values and returns a list
-- of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate = undefine

filterDbDate = foldr f []
    where f (DbDate t) xs = t : xs
          f _          xs = xs

-- filterDbDate = foldr f [] theDatabase
-- (1911-05-01 09:28:43 UTC) : (foldr f [] )

-- 2. Write a function that filters for DbNumber values and returns a
-- list of the Integer values inside them

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f (DbNumber t) xs = t : xs
          f _            xs = xs

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
-- mostRecent = undefined
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
-- sumDb = undefined
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values.
-- -- You'll probably need to use fromIntegral
-- -- to get from Integer to Double.

avgDb :: [DatabaseItem] -> Double
-- avgDb = undefined
avgDb xs = (fromIntegral .sumDb) xs / (fromIntegral . length  . filterDbNumber) xs