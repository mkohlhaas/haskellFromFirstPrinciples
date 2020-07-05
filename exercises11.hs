module Exercises18 where

import Data.Char as Char

data Weekday =
   Monday
 | Tuesday
 | Wednesday
 | Thursday
 | Friday

f Friday = "Miller Time"

diffChars :: Char -> Char -> Int
diffChars a b = ord a - ord b

shiftChars :: Char -> Char -> Char
shiftChars s k = chr((((diffChars s 'A') + (diffChars k 'A')) `mod` 26) + ord 'A')

vigenere :: String -> String -> String
vigenere str key = vigenere' str key
                     where vigenere' [] _ = []
                           vigenere' (s : ss) (k : ks) =
                               if isSpace s
                               then s : vigenere' ss (k:ks)
                               else shiftChars s k : vigenere' ss (if null ks then key else ks)

testVigenere = vigenere "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (a : as) all@(b : bs) = elem a all && isSubseqOf as (tail(dropWhile (/= a) all))

-- capitalizeWords :: String -> [(String, String)]
-- capitalizeWords str = capitalizeWords' (words str)
--                         where capitalizeWords' [] = []
--                               capitalizeWords' (word : words) = (word, capitalize word) : capitalizeWords' words
--                               capitalize word = toUpper(head word) : tail word

capitalize :: String -> String
capitalize word = toUpper(head word) : tail word      

capitalizeWords = map (\a -> (a, capitalize a)) . words

--------------------------------------------------------------------------------------------------------------------------------

unsentence :: [String] -> String
unsentence = concat . map (++ ['.'])

sentences :: String -> [String]
sentences "" = []
sentences s = firstSentence : restOfSentences
                where (firstSentence, ss) = (break (== '.') s)
                      restOfSentences = sentences(tail(ss))

capitalizeFirstWord :: String -> String
capitalizeFirstWord word = nonAlphaChars ++ capitalizedWord
                           where (nonAlphaChars, cs) = break isAlpha word
                                 capitalizedWord = (toUpper(head(cs)) : tail(cs))

capitalizeSentences :: String -> String
capitalizeSentences s = unsentence $ map capitalizeFirstWord (sentences s)

--------------------------------------------------------------------------------------------------------------------------------

