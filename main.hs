
-- Do not alter the following line
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BlockArguments #-}
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char,
drop_char, encode, complex_encode, complex_decode) where
import Distribution.Simple.Setup (falseArg, trueArg)
import Distribution.Simple.Utils (xargs)
-- Part A
char_to_int :: Char -> Integer
--char_to_int = error "Not implemented"
char_to_int '0'= 0
char_to_int '1'= 1
char_to_int '2'= 2
char_to_int '3'= 3
char_to_int '4'= 4
char_to_int '5'= 5
char_to_int '6'= 6
char_to_int '7'= 7
char_to_int '8'= 8
char_to_int '9'= 9
char_to_int _ = error "Invalid input."
repeat_char :: Char -> Integer -> String
--repeat_char = error "Not implemented"
repeat_char _ 0 = ""
repeat_char c n = c : repeat_char c (n-1)
decode :: String -> String
--decode = error "Not implemented"
decode [] = ""
decode (x:y:xs) = repeat_char x (char_to_int y) ++ decode xs
decode (_:_) = error "Invalid input."
-- Part B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
int_to_char :: Integer -> Char
--int_to_char = error "Not implemented"
int_to_char 0 = '0'
int_to_char 1 = '1'
int_to_char 2 = '2'
int_to_char 3 = '3'
int_to_char 4 = '4'
int_to_char 5 = '5'
int_to_char 6 = '6'
int_to_char 7 = '7'
int_to_char 8 = '8'
int_to_char 9 = '9'
int_to_char _ = error "Invalid input."
length_char :: Char -> String -> Integer
--length_char = error "Not implemented"
length_char _ [] = 0
length_char c (x:xs) = if c == x then
                        1 + length_char c xs
                       else
                        0
drop_char :: Char -> String -> String
--drop_char = error "Not implemented"
drop_char _ [] = []
drop_char c (x:xs) = if c == x then
                        drop_char c xs
                     else
                        x:xs
encode :: String -> String
--encode = error "Not implemented"
encode [] = []
encode (x:xs) = let length = length_char x xs + 1
                in x : int_to_char length : encode (drop_char x xs)

-- Part C
intToChar :: Integer -> String
intToChar x 
    | x == 0 = ['0']
    | x == 1 = ['1']
    | x == 2 = ['2']
    | x == 3 = ['3']
    | x == 4 = ['4']
    | x == 5 = ['5']
    | x == 6 = ['6']
    | x == 7 = ['7']
    | x == 8 = ['8']
    | x == 9 = ['9']
    | x > 9 = (intToChar (x `div` 10)  ++ intToChar (mod x 10 ) )

elem' :: Char -> String -> Bool
elem' e [] = False
elem' e (x:xs)
   | e == x = True
   | otherwise = elem' e xs

length' [] = 0
length' (_:xs) = 1 + length' xs

drop' 0 list = list
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs

isNumber :: Char -> Bool
isNumber x = elem' x "0123456789"

extractNumberHelper [] y = y
extractNumberHelper (x:xs) y
                  | isNumber x = extractNumberHelper xs (y ++ [x])
                  | y == "" = extractNumberHelper xs y
                  | otherwise = y

extractNumber :: String -> String
extractNumber str = extractNumberHelper str ""

convertInteger :: Char -> Integer
convertInteger '0' = 0
convertInteger '1' = 1
convertInteger '2' = 2
convertInteger '3' = 3
convertInteger '4' = 4
convertInteger '5' = 5
convertInteger '6' = 6
convertInteger '7' = 7
convertInteger '8' = 8
convertInteger '9' = 9
convertInteger _   = error "Invalid"

stringToIntHelper [] y = y
stringToIntHelper (x:xs) y
                  | isNumber x = stringToIntHelper xs (y * 10 + convertInteger x)
                  | otherwise = error "test"

stringToInt :: String -> Integer
stringToInt str = stringToIntHelper str 0

complex_encode :: String -> String
--complex_encode = error "Not implemented"
complex_encode [] = []
complex_encode (x:xs) = if length_char x xs == 0 
                        then x : complex_encode (drop_char x xs) 
                        else let length = length_char x xs + 1 in x : intToChar length ++ complex_encode (drop_char x xs)

complex_decode :: String -> String
complex_decode [] = []
complex_decode [x] = [x]
complex_decode (x:xs) = if isNumber (head xs)
                        then repeat_char x (stringToInt (extractNumber xs)) ++ complex_decode (drop' (length' (extractNumber xs)) xs)
                        else x : complex_decode xs
