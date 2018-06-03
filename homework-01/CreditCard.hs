{-# OPTIONS_GHC -Wall #-}


module CreditCard where

---------------------------------- Exercise 1 ----------------------------------

-- Convert an integer to a list of digits
toDigits:: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Same as toDigits but with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)


toDigitsTest :: Bool
toDigitsTest = and
  [
    [1] == toDigits 1,
    [1, 2, 3, 4] == toDigits 1234,
    [] == toDigits 0,
    [] == toDigits (-17)
  ]

toDigitsRevTest :: Bool
toDigitsRevTest = and
  [
    [1] == toDigitsRev 1,
    [4, 3, 2, 1] == toDigitsRev 1234,
    [] == toDigitsRev 0,
    [] == toDigitsRev (-17)
  ]


---------------------------------- Exercise 2 ----------------------------------

-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse


doubleEveryOtherTest :: Bool
doubleEveryOtherTest = and
  [
    [5] == doubleEveryOther [5],
    [10, 5] == doubleEveryOther [5, 5],
    [1, 4, 3] == doubleEveryOther [1, 2, 3],
    [16, 7, 12, 5] == doubleEveryOther [8, 7, 6, 5]
  ]


---------------------------------- Exercise 3 ----------------------------------

-- Returns the sum of all digits of each number of the list
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits


sumDigitsTest :: Bool
sumDigitsTest = and
  [
    5 == sumDigits [5],
    10 == sumDigits [5, 5],
    22 == sumDigits [16, 7, 12, 5]
  ]


---------------------------------- Exercise 4 ----------------------------------

-- Indicates whether or not a credit card number is valid
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0


validateTest :: Bool
validateTest = and
  [
    True == validate 4012888888881881,
    False == validate 4012888888881882
  ]
