{-# LANGUAGE FlexibleInstances #-}

module Algebraic where
import Data.Int

data DogueDeBordeaux doge = DogueDeBordeaux doge
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)
type Size = Integer

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer

class TooMany a where
  tooMany :: a -> Bool
instance TooMany Int where
  tooMany n = n > 42
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42
-- instance TooMany (Int, Int) where
--   tooMany (n, m) = n + m

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
data Person = Person { name :: String , age :: Int } deriving (Eq, Show)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)
type Name = String
type Age = Int
type LovesMud = Bool
type Awesome = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter = Twitter deriving (Eq, Show)
-- data AskFm = AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork = Twitter | AskFm deriving (Eq, Show)

myRecord  :: RecordProduct Integer Float
myRecord  =  RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' =  RecordProduct { pfirst = 42 , psecond = 0.00001 }

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem , lang :: ProgLang } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]


