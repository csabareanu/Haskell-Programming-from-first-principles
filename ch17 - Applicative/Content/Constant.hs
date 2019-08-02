module Constant where


-- -- f ~ Constant e
-- (<*>) ::          f (a -> b) ->          f a ->          f b
-- (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b

-- pure :: a ->          f a
-- pure :: a -> Constant e a


-- *Main> Constant (Sum 1) <*> Constant (Sum 2)
-- Constant {getConstant = Sum {getSum = 3}


-- Prelude> Constant undefined <*> Constant (Sum 2)
-- Constant (Sum {getSum = *** Exception: Prelude.undefined
-- *Main> pure 1
-- 1
-- *Main> pure 1 :: Constant String Int
-- Constant {getConstant = ""}