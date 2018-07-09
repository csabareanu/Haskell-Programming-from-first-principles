module OrdInstances where

-- default Ord Instance (must have Eq Instance - superclass of Ord)
--data DayOfWeek =
--    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Ord, Eq, Show)

-- custom Ord Instance with Fri the greatest and else equal
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Eq, Show)

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _ Fri   = LT
    compare _ _     = EQ
