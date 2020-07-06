module DayOfWeek where

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show, Ord)
data Date = Date DayOfWeek Int deriving Show
data Identity a = Identity a
data TisAnInteger = TisAn Integer deriving Show
data TwoIntegers = Two Integer Integer deriving Show
data StringOrInt = TisAnInt Int | TisAString String deriving Show
data Pair a = Pair a a deriving Show
data Tuple a b = Tuple a b deriving Show
data Which a = ThisOne a | ThatOne a deriving Show
data EitherOr a b = Hello a | Goodbye b deriving Show
data Mood = Blah

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn m) = n == m

instance Eq TwoIntegers where
  (==) (Two a b ) (Two a' b') = a == a' && b == b'

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  (==) _ _ = False

instance Show Mood where
  show _ = "Blahing"

f :: Int -> Bool
f 2 = True
f 3 = True
f _ = False
