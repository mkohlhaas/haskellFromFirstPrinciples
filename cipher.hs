module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar [] _ = []
caesar (c : cs) shift = chr (ord c + shift) : caesar cs shift

uncaesar :: String -> Int -> String
uncaesar [] _ = []
uncaesar (c : cs) shift = chr (ord c - shift) : uncaesar cs shift
