module Maybe2 where

import Control.Applicative

data Cow = Cow {
      name :: String
    , age  :: Int
    , weight :: Int
}   deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""    = Nothing
noEmpty str   = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0     = Just n
    | otherwise  = Nothing


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w =
    case noEmpty n of
        Nothing -> Nothing
        Just n ->
            case noNegative a of
                Nothing -> Nothing
                Just a ->
                    case noNegative w of
                        Nothing -> Nothing
                        Just w -> Just (Cow n a w)

-- improved with Applicative
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w
-- *Maybe2> let cow1 = Cow <$> noEmpty "Bess"
-- *Maybe2> :t cow1
-- cow1 :: Maybe (Int -> Int -> Cow)
-- *Maybe2> let cow2 = cow1 <*> noNegative 1
-- *Maybe2> :t cow2
-- cow2 :: Maybe (Int -> Cow)
-- *Maybe2> let cow3 = cow2 <*> noNegative 2
-- *Maybe2> :t cow3
-- cow3 :: Maybe Cow


cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' n a w = liftA3 Cow (noEmpty n) (noNegative a) (noNegative w)
-- *Maybe2> :t Cow
-- Cow :: String -> Int -> Int -> Cow
-- *Maybe2> let cow1 = liftA3 Cow
-- *Maybe2> :t cow1
-- cow1 :: Applicative f => f String -> f Int -> f Int -> f Cow
-- *Maybe2> let cow2 = cow1 (noEmpty "Bess")
-- *Maybe2> :t cow2
-- cow2 :: Maybe Int -> Maybe Int -> Maybe Cow
-- *Maybe2> let cow3 = cow2 (noNegative 1)
-- *Maybe2> :t cow3
-- cow3 :: Maybe Int -> Maybe Cow
-- *Maybe2> cow4 = cow3 (noNegative 3)
-- *Maybe2> :t cow4
-- cow4 :: Maybe Cow
-- *Maybe2> cow4
-- Just (Cow {name = "Bess", age = 1, weight = 3})
