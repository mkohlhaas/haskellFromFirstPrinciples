module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  -- | otherwise = error "Not a valid digit."

digits :: Int -> [Int]
digits n = digits' n []
           where digits' n acc
                   | n > 10 = digits' (div n 10) ((mod n 10) : acc)
                   | otherwise =  n : acc

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits 
