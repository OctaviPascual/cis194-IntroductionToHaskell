{-# OPTIONS_GHC -Wall #-}

module Lecture where

-- Compute the sum of the integers from 1 to n
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- Compute the next number in the hailstone sequence
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

-- Generate the sequence of hailstone iterations from a starting number
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- The number of hailstone steps needed to reach 1 from a starting number
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

-- Compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

-- Generate a list by summing every two numbers of the original one
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []       = []
sumEveryTwo (x:[])   = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs
