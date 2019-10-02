module Either where

import Data.Validation

-- f ~ Either e
-- (<*>) :: f        (a -> b) -> f        a -> f        b
-- (<*>) :: Either e (a -> b) -> Either e a -> Either e b

-- pure :: a -> f        a
-- pure :: a -> Either e a

-- data Validation err a =
--     Failure err
-- --     | Success a
--     deriving (Eq, Show)


-- Natural Transformations
-- validToEither :: Validation e a -> Either e a
-- validToEither (Failure err) = Left err
-- validToEither (Success a) = Right a

-- eitherToValid :: Either e a -> Validation e a
-- eitherToValid (Left err) = Failure err
-- eitherToValid (Right a) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id

data Errors =
    DividedByZero
    | StackOverflow
    | MooglesChewedWires
    deriving (Eq, Show)

-- success = Success (+1) <*> Success 1
-- -- success == Success 2
-- failure = Success (+1) <*> Failure [StackOverflow]
-- -- failure == Failure [StackOverflow]
-- failure' = Failure [StackOverflow] <*> Success (+1)
-- -- failure' == Failure [StackOverflow]
-- failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
-- -- failures == Failure [MooglesChewedWires, StackOverflow]