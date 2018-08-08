module ConsDecons where

data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                   ,psecond :: b }
                    deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
-- Sheep can produce between 2 and 30
-- pounds (0.9 and 13 kilos) of wool per year!
-- Icelandic sheep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool.
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

-- Alternately
type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)



--Constructing values

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a->a)
idIdentity = MkId $ \x -> x

type Awesome = Bool
-- type Name = String

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter =
--     Twitter deriving (Eq, Show)

-- data AskFm =
--     AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork =
    Twitter
    | AskFm
    deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "AskFm"

-- Try to avoid using type synonyms with unstructured
-- data like text or binary. Type synonyms are best used when you
-- want something lighter weight than newtypes but also want your type
-- signatures to be more explicit.

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42
                           ,psecond = 0.000001}

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

--deconstruction
newtype NameF = NameF String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer =
    Farmer NameF Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

data FarmerRec =
    FarmerRec {
          name :: Name
        , acres :: Acres
        , farmerType :: FarmerType
    }
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _           -> False
