module Funcy where

myNum :: Integer
myNum = 1

myVal f g = myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

-- bindExp :: Integer -> String
-- bindExp x =
--   let z = y + x in
--     let y = 5 in
--       "the integer was: "
--       ++ show x ++ " and y was: "
--       ++ show y ++ " and z was: "
--       ++ show z

bindExp1 :: Integer -> String
bindExp1 x =
  let x = 10; y = 5 in
     "the integer was: " ++ show x
     ++ " and y was: " ++ show y

addOneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
-- isItTwo _ = False
