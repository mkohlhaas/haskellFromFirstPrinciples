module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
-- roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main = do
  print (roundTrip 4 :: Int)
  print (id 4)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

-- uncurried function,
-- takes a tuple of its arguments
add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' = curry' add

