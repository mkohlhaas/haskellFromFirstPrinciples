module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

myUser = Username "callen"
myAcct = AccountNumber 10456

data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Penguin WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Penguin whereitlives) = whereitlives 

humboldt = Penguin SouthAmerica
gentoo = Penguin Antarctica
macaroni = Penguin Antarctica
little = Penguin Australia
galapagos = Penguin Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Penguin Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Penguin Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ :: (Eq a, Num a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

palindrome xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True  -> putStrLn "Eyyyyy. What's shakin'?"
    False -> putStrLn "Pshhhh."
  where cool = coolness == "downright frosty yo"

-- functionC x y = if (x > y) then x else y
functionC x y = case x > y of
  True -> x
  _ -> y

-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n = case even n of
  True -> n + 2
  _ -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

