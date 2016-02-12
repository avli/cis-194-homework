{-# OPTIONS_GHC -Wall #-}

module Creditcard where

charToString :: Char -> String
charToString ch = [ch]

readDigit :: Char -> Integer
readDigit x = read $ charToString x

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map readDigit x'
    where x' = show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:zs) =
  [x, y+y] ++ doubleEveryOther zs
doubleEveryOther [x] = [x]
doubleEveryOther _ = []

sumDigits :: [Integer] -> Integer
sumDigits xs = sum . concat $ map toDigits xs

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigitsRev $ x) `rem` 10 == 0
