module PersonValidator where

type Age = Integer
type Name = String

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
    True ->  Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
    True ->  Right name
    False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
    mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOkay) (Right ageOk) = Right (Person nameOkay ageOk)
mkPerson' (Left badName) (Left badAge)   = Left (badName ++ badAge)
mkPerson' (Left badName) _               = Left (badName)
mkPerson' _              (Left badAge)   = Left (badAge)