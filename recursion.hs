module Recursion where

factorial :: Integer -> Integer
factorial n | n < 0 = error "Argument to factorial must be a positive integer."
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

-- applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
-- applyTimes 0 _ b = b
-- applyTimes n f b = f (applyTimes (n-1) f b)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  let go n count
        | n < denom = (count, n)
        | otherwise = go (n - denom) (count + 1)
  in go num 0

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sums :: (Eq a, Num a) => a -> a
sums n = 
  let sums' 0 result = result 
      sums' n result = sums' (n - 1) (result + n)
  in sums' n 0

myMult :: Integral a => a -> a -> a
myMult a b = myMult' a b 
             where myMult' 0 _ = 0
                   myMult' _ 0 = 0
                   myMult' n 1 = n
                   myMult' n m = myMult' (n + a) (m - 1)

data DividedResult a = Result a | DividedByZero deriving Show

myDividedBy :: Integral a => a -> a -> DividedResult a
myDividedBy num denom
  | denom == 0 = DividedByZero
  | otherwise = Result (div num denom)

mc91 n | n > 100 = n -10
       | otherwise = mc91 $ mc91 (n + 11)
