module Exercises12 where

import Data.List

notThe :: String -> Maybe String
notThe str =
  case "the" /= str of
    True -> Just str
    _    -> Nothing

-- replaceThe :: String -> String
replaceThe str =
  unwords $
  map (\m -> case m of
               Nothing  -> "a"
               Just str -> str)
             $ map notThe $ words str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = ctbv mots
    where mots = words str
          ctbv [] = 0
          ctbv ("the" : rest) = ctbvr rest
          ctbv (_ : rest) = ctbv rest
          ctbvr (w : rest) | head w == 'a' || head w == 'e' || head w == 'i' || head w == 'o' || head w == 'u' = 1 + ctbv rest
          ctbvr ws = ctbv ws

isVowel :: Char -> Bool
isVowel c | c == 'a' || c == 'e' || c == 'i' || c == 'o' ||  c == 'u' = True
isVowel _ = False

-- countVowels :: String -> Integer
countVowels = length . filter (== True) . map isVowel

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if countVowels str > length str - countVowels str then Nothing else Just $ Word' str

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

-- integerToNat :: Integer -> Maybe Nat
integerToNat int | int < 0 = Nothing
integerToNat int = Just $ integerToNat' int
                     where integerToNat' 0 = Zero
                           integerToNat' int = (Succ $ integerToNat' (int - 1))

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust _        = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : rest) = catMaybes rest
catMaybes (Just a : rest)  = a : catMaybes rest

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe lst | containsNothing lst = Nothing
flipMaybe lst                       = Just $ catMaybes lst

containsNothing []            = False
containsNothing (Nothing : _) = True
containsNothing (_ : rest)    = containsNothing rest

lefts :: [Either a b] -> [a]
lefts = foldr (\a b -> case a of
                         (Left val) -> val : b
                         _          -> b)
              [] 

rights :: [Either a b] -> [b]
rights = foldr (\a b -> case a of
                         (Right val) -> val : b
                         _           -> b)
               [] 

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (\a (ls, rs) -> case a of
                                           (Left lval)  -> (lval : ls, rs)
                                           (Right rval) -> (ls, rval : rs))
                         ([], [])

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _)  = Nothing
eitherMaybe f (Right b) = Just $ f b
 
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = either' (\a -> Nothing) (\b -> Just $ f b) e

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> [] 
                  (Just (a', b')) -> a' : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just(a, f a))

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing             -> Leaf
               (Just (a', b, a'')) -> Node b (unfold f a') (unfold f a'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> case a of
                              0 -> Nothing
                              _ -> Just(a - 1, n - a, a - 1))
              n
