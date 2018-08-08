module ConsDecons_interm where

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer {os :: OperatingSystem
               ,lang :: ProgrammingLanguage}
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac
                        ,lang = Haskell}

feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda
                             ,os = GnuPlusLinux}


-- Write a function that generates all possible values of Programmer . Use
-- the provided lists of inhabitants of OperatingSystem and Program-
-- mingLanguage .
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
-- allProgrammers = undefined
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

-- *ConsDecons_interm> length allOperatingSystems * length allLanguages == length allProgrammers
-- True

-- Since
-- Programmer is a product of OperatingSystem and Program-
-- mingLanguage , you can determine how many inhabitants of Program-
-- mer you have by calculating:
-- length allOperatingSystems * length allLanguages
-- This is the essence of how product types and the number of inhabitants
-- relate.
-- If after running nub from Data.List to remove duplicate values over
-- your allProgrammers value, it equals the number returned by multi-
-- plying those lengths together, you’ve probably got it figured out. Try
-- to be clever and make it work without manually typing out the values.