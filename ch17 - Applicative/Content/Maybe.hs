module Maybe where

-- -- f ~ Maybe
-- (<*>) ::     f (a -> b) ->     f a ->     f b
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

-- pure :: a ->     f a
-- pure :: a -> Maybe a

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
-- Name :: (String -> Name) and is acting as a function waiting a Name
-- validateLength 25 "Andrei" = Just "Andrei" (bc it has less than 25 chars)
-- fmap Name (Just "Andrei")
-- Just (Name "Andrei")

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person =
    Person Name Address
    deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing
                Just a' ->
                    Just $ Person n' a'