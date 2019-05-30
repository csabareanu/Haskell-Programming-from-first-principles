module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "wohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- *ReplaceExperiment> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f b -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe[Char]] -> [Char]
liftedReplace' = liftedReplace

-- Explanation: The f in f Char is the [] around Char (the structure we lifted over)
--              f ~ []

-- Lifting Twice
-- *ReplaceExperiment> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe[Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f1 ~ []
-- f2 ~ Maybe

-- Lifting Three Times
-- *ReplaceExperiment> :t (fmap.fmap.fmap) replaceWithP
-- (fmap.fmap.fmap) replaceWithP :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe[Char]] -> [Maybe[Char]]
thriceLifted' = thriceLifted
-- f1 ~ []
-- f2 ~ Maybe
-- f3 ~ []

main :: IO ()
main = do
    putStrLn "replaceWithP' lms:    "
    print (replaceWithP lms)

    putStrLn "liftedReplace lms:    "
    print (liftedReplace lms)

    putStrLn "liftedReplace' lms:    "
    print (liftedReplace' lms)

    putStrLn "twiceLifted lms:    "
    print (twiceLifted lms)

    putStrLn "twiceLifted' lms:    "
    print (twiceLifted' lms)

    putStrLn "thriceLifted lms:    "
    print (thriceLifted lms)

    putStrLn "thriceLifted' lms:    "
    print (thriceLifted' lms)
