module Higher_Order_Funcs where

-- HOFs are functions that accept functions as args. Used to manipulate how funcs are applied to args

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x 

myFlip' :: (a -> b -> c) -> b -> a -> c
myFlip' f = \x y -> f y x 


data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'


employeeRank :: Employee -> Employee -> IO()
employeeRank e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Employee is the boss"
        LT -> reportBoss e' e


codersRuleCEOsDroll :: Employee -> Employee -> Ordering
codersRuleCEOsDroll Coder Coder = EQ
codersRuleCEOsDroll _     Coder = LT
codersRuleCEOsDroll Coder _     = GT
codersRuleCEOsDroll e     e'    = compare e e'

employeeRank2 :: (Employee -> Employee -> Ordering)
                -> Employee
                -> Employee
                -> IO ()

employeeRank2 f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither Employee is the Boss "
        LT -> flip reportBoss e e'

ranking = employeeRank2 codersRuleCEOsDroll Coder CEO
