module SurprisingFunctors where

import Data.Functor.Const

newtype Constant a b =
         Constant { getConstant :: a }
         deriving (Eq, Show)

-- parameter b is a phantom type

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

-- In this functor, the function will never be applied , and fmap will ALWAYS return the functor given
-- Does it adhere to the Functor Laws ?

---------------
-- Identity
---------------
main = do
    putStr "Identity: "
    -- fmap id == id
    print $ getConstant (id (Constant 3)) == getConstant (fmap id (Constant 3))

    putStrLn ""
    putStrLn "Composition: "
    -- fmap (f . g) == (fmap f) . (fmap g)
    let left = getConstant $ fmap ((const 5) . (const 10)) $ Constant "WoHoo"
    let right = getConstant $ (fmap (const 5)) . (fmap (const 10)) $ Constant "WoHoo"

    print $ left == right