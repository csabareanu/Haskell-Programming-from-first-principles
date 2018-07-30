module DataTypes_interm where

-- Given the datatypes defined in the above sections,

-- 1. Is Doggies a type constructor or a data constructor?
-- Type Constructor


-- 2. What is the kind of Doggies?
-- * -> *  //needs to be applied to become a concrete type


-- 3. What is the kind of Doggies String?
-- Doggies String :: *


-- 4. What is the type of Husky 10?
-- Husky 10 :: Num a => Doggies a


-- 5. What is the type of Husky (10 :: Integer)?
-- Husky (10 :: Integer) :: Doggies Integer


-- 6. What is the type of Mastiff "Scooby Doo"?
-- Mastiff "Scooby Doo" :: Doggies [Char]


-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- Both, depending on where it is used


-- 8. What is the type of DogueDeBordeaux?
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge


-- 9. What is the type of DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]