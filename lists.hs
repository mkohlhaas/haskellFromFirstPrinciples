module Lists where

import Data.Char

myWords :: [Char] -> [[Char]]
myWords lst = myWords' lst []
              where
                myWords' [] acc = filter (/= "") $ reverse acc
                myWords' lst acc = myWords' (dropWhile (/= ' ') (dropWhile (== ' ') lst))
                                            (takeWhile (/= ' ') (dropWhile (== ' ') lst) : acc)

splitOn :: [Char] -> Char -> [[Char]]
splitOn str char = splitOn' str []
              where
                splitOn' []  acc = filter (/= "") $ reverse acc
                splitOn' str acc = splitOn' (dropWhile (/= char) (dropWhile (== char) str))
                                            (takeWhile (/= char) (dropWhile (== char) str) : acc)

mySqr  = [x^2 | x <- [1..5] ]
myCube = [y^3 | y <- [1..5] ]

sqrCubes = length [(x, y) | x <- mySqr, y <- myCube, x < 450, y < 50 ]

myzip :: [a] -> [b] -> [(a, b)]
myzip [] _  = []
myzip _  [] = []
myzip (a : as) (b : bs) = (a, b) : myzip as bs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] _  = [] 
myzipWith f _  [] = [] 
myzipWith f (a : as) (b : bs) = f a b : myzipWith f as bs

strToUpper :: String -> String
strToUpper []  = []
strToUpper str = (toUpper . head) str : tail str

holler :: String -> String
holler [] = []
holler (c : chars) = toUpper c : holler chars

firstLetterUppercase :: String -> Maybe Char
firstLetterUppercase []      = Nothing 
firstLetterUppercase (c : _) = Just $ toUpper c

firstLetterUp = toUpper . head

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b : bs) = b && myAnd bs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b : bs) = b || myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (a : as) = f a || myAny f as

-- myElem :: Eq a => a -> [a] -> Bool
-- myElem n [] = False
-- myElem n (m : ms) = n == m || myElem n ms

myElem :: Eq a => a -> [a] -> Bool
myElem n lst = any (\m -> m == n) lst

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a : as) = myReverse as ++ [a]

squish :: [[a]] -> [a]
squish [] = []
squish (lst : lsts) = lst ++ squish lsts 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a : as) = f a ++ squishMap f as

squishAgain :: [[a]] -> [a]
squishAgain lol = squishMap id lol

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []  = error "Maximum of an empty list is undefined"
myMaximumBy _ [a] = a
myMaximumBy f (a : as) = let maxRest = myMaximumBy f as in
                           if f a maxRest == GT
                             then a
                             else maxRest

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []  = error "Minimum of an empty list is undefined"
myMinimumBy _ [a] = a
myMinimumBy f (a : as) = let minRest = myMinimumBy f as in
                           if f a minRest == LT
                             then a
                             else minRest

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
