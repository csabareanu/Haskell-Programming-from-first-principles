module FunctionType where

-- The type of functions (->) is, in the algebra of types, the exponentiation operator.

data Quantum =
      Yes
    | No
    | Both
    deriving (Eq, Show)

-- 3 + 3 = 6
quantumSum1 :: Either Quantum Quantum
quantumSum1 = Right Yes

quantumSum2 :: Either Quantum Quantum
quantumSum2 = Right No

quantumSum3 :: Either Quantum Quantum
quantumSum3 = Right Both

quantumSum4 :: Either Quantum Quantum
quantumSum4 = Left Yes

quantumSum5 :: Either Quantum Quantum
quantumSum5 = Left No

quantumSum6 :: Either Quantum Quantum
quantumSum6 = Left Both

-- 3 * 3 = 9
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- 3^3 = 27
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No  = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No  = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No  = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No  = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No  = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes = No
quantFlip6 No  = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes = Both
quantFlip7 No  = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes = Both
quantFlip8 No  = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes = Both
quantFlip9 No  = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes = Both
quantFlip10 No  = No
quantFlip10 Both = Both

---- etc.