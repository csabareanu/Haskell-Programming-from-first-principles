module Record where

data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

-- this is equivalent to Person. Using Record syntax
data Person2 =
    Person {
          name :: String
        , age :: Int
    }
    deriving (Eq, Show)

-- *Record> Person "Papu" 5
-- Person {name = "Papu", age = 5}

-- *Record> let papu = Person "Papu" 5
-- *Record> age papu
-- 5
-- *Record> name papu
-- "Papu"
-- *Record>

jm2 = Person "julie" 108
ca2 = Person "chris" 16