module Phone where

type Presses = String
type PhoneTouches = [(Char, Presses)]

data DaPhone = DaPhone PhoneTouches deriving Show

phone = DaPhone -- [('1',"1" ),('2', "ABC2"),('3', "DEF3"),('4', "GHI4"),('5', "JKL5"),('6', "MNO6"),('7', "PQRS7"),('8', "TUV8"),('9', "WXYZ9"),('*', "^"),('0', " +-0"),('#', ".,#")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

convertMessage :: String -> Presses
convertMessage = concat . map phoneTouches

phoneTouches :: Char -> Presses
phoneTouches c = case c of
   '1' -> "1"
   '2' -> "2222"
   '3' -> "3333"
   '4' -> "4444"
   '5' -> "5555"
   '6' -> "6666"
   '7' -> "77777"
   '8' -> "8888"
   '9' -> "99999"
   '0' -> "0000"
   ' ' -> "0"
   'a' -> "2"
   'b' -> "22"
   'c' -> "222"
   'd' -> "3"
   'e' -> "33"
   'f' -> "333"
   'g' -> "4"
   'h' -> "44"
   'i' -> "444"
   'j' -> "5"
   'k' -> "55"
   'l' -> "555"
   'm' -> "6"
   'n' -> "66"
   'o' -> "666"
   'p' -> "7"
   'q' -> "77"
   'r' -> "777"
   's' -> "7777"
   't' -> "8"
   'u' -> "88"
   'v' -> "888"
   'w' -> "9"
   'x' -> "99"
   'y' -> "999"
   'z' -> "9999"
   'A' -> "*2"
   'B' -> "*22"
   'C' -> "*222"
   'D' -> "*3"
   'E' -> "*33"
   'F' -> "*333"
   'G' -> "*4"
   'H' -> "*44"
   'I' -> "*444"
   'J' -> "*5"
   'K' -> "*55"
   'L' -> "*555"
   'M' -> "*6"
   'N' -> "*66"
   'O' -> "*666"
   'P' -> "*7"
   'Q' -> "*77"
   'R' -> "*777"
   'S' -> "*7777"
   'T' -> "*8"
   'U' -> "*88"
   'V' -> "*888"
   'W' -> "*9"
   'X' -> "*99"
   'Y' -> "*999"
   'Z' -> "*9999"
   '-' -> "000"
   '#' -> "###"
   '.' -> "#"
   ',' -> "##"
