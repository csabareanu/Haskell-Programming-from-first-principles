module SurprisingFunctors where

import Data.Functor.Const

--newtype Constant a b =
--          Constant { getConstant :: a }
--          deriving (Eq, Show)


-- instance Functor (Constant m) where
--     fmap _ (Constant v) = Constant v

-- In this functor, the function will never be applied , and fmap will ALWAYS return the functor given
-- Does it adhere to the Functor Laws ?

---------------
-- Identity
---------------
main = do
    putStr "Identity: "
    -- fmap id == id
    print $ getConst (id (Const 3)) == getConst(fmap id (Const 3))

    putStrLn ""
    putStrLn "Composition: "
    -- fmap (f . g) == (fmap f) . (fmap g)
    print $