module Folds where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
 , DbNumber 9001
 , DbString "Hello, world!"
 , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate utcTime : rest) = utcTime : filterDbDate rest 
filterDbDate (_ : rest) = filterDbDate rest

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber n : rest) = n : filterDbNumber rest
filterDbNumber (_ : rest) = filterDbNumber rest

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length db)

factorial :: Integer -> Integer
factorial n = last $ scanl (*) 1 [1..n]

stops = "pbtdkg"
vowels = "aeiou"

svs :: [(Char, Char, Char)]
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- seekritFunc :: String -> Int
seekritFunc :: Fractional a => String -> a
seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\m n -> if f m then True else n) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\m n -> if m == el then True else n) False

myReverse :: [a] -> [a]
-- myReverse = foldl (flip (:)) []
myReverse = foldr (\m n -> n ++ [m]) [] 

squish :: [[a]] -> [a]
squish = foldr (\m n -> m ++ n) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\m n -> f m ++ n) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "An empty list makes no sense here!"
myMaximumBy f (a : as) = foldr (\m n -> if f m n == GT then m else n) a as

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = error "An empty list makes no sense here!"
myMinimumBy f (a : as) = foldr (\m n -> if f m n == LT then m else n) a as
