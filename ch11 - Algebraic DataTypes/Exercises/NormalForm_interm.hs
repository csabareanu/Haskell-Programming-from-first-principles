module NormalForm_interm where


-- 1. Given the type
-- data FlowerType = Gardenia
--         | Daisy
--         | Rose
--         | Lilac
--         deriving Show
type Gardener = String

-- data Garden = Garden Gardener FlowerType
--                 deriving Show


-- What is the normal form of Garden?
data GardenNF = Gardenia Gardener
                | Daisy Gardener
                | Rose Gardener
                | Lilac Gardener
                deriving Show