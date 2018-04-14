{-# OPTIONS_GHC -Wall #-}

module CreditCard where

-- Convert an integer to a list of digits
toDigits:: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsTest :: Bool
toDigitsTest = and
  [
    [1] == toDigits 1,
    [1, 2, 3, 4] == toDigits 1234,
    [] == toDigits 0,
    [] == toDigits (-17)
  ]


-- Same as toDigits but with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigitsRevTest :: Bool
toDigitsRevTest = and
  [
    [1] == toDigitsRev 1,
    [4, 3, 2, 1] == toDigitsRev 1234,
    [] == toDigitsRev 0,
    [] == toDigitsRev (-17)
  ]


-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) | even (length zs) = 2*x : y : doubleEveryOther zs
doubleEveryOther (x:y:zs)                    = x : 2*y : doubleEveryOther zs

doubleEveryOtherTest :: Bool
doubleEveryOtherTest = and
  [
    [5] == doubleEveryOther [5],
    [10, 5] == doubleEveryOther [5, 5],
    [1, 4, 3] == doubleEveryOther [1, 2, 3],
    [16, 7, 12, 5] == doubleEveryOther [8, 7, 6, 5]
  ]


-- Returns the sum of all digits of each number of the list
-- The list contains a mix of one-digit and two-digit numbers
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

sumDigitsTest :: Bool
sumDigitsTest = and
  [
    5 == sumDigits [5],
    10 == sumDigits [5, 5],
    22 == sumDigits [16, 7, 12, 5]
  ]


-- Indicates whether or not a credit card number is valid
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

validateTest :: Bool
validateTest = and
  [
    True == validate 4012888888881881,
    False == validate 4012888888881882
  ]
