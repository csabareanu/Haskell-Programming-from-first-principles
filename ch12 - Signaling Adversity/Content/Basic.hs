module Basic where

-- data Maybe a = Nothing | Just a

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- smart Constructor. Constructs values of a certain type only when we meet certain criteria.

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >=0 = Just $Person name age
    | otherwise             = Nothing

-- data Either a b = Left a | Right b

-- we derive Eq because guards need this instance.
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

data Person2 = Person2 Name Age deriving Show

mkPerson2 :: Name -> Age -> Either PersonInvalid Person2
mkPerson2 name age
    | name /= "" && age >=0 = Right $Person2 name age
    | name == ""            = Left NameEmpty
    | otherwise             = Left AgeTooLow

-- if both name and age are invalid the f above will return the first checked.
