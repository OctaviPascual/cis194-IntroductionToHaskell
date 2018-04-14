{-# OPTIONS_GHC -Wall #-}

module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Return the list of moves to be performed to move n discs from the source
-- peg to the destination peg using one auxiliary peg:
--     1. move n−1 discs from src to aux using dst as temporary storage
--     2. move the top disc from src to dst
--     3. move n−1 discs from aux to dst using src as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dst aux =
  moveFromSrcToAux ++ moveFromSrcToDst ++ moveFromAuxToDst
  where
    moveFromSrcToAux = hanoi (n-1) src aux dst
    moveFromSrcToDst = [(src, dst)]
    moveFromAuxToDst = hanoi (n-1) aux dst src

hanoiTest :: Bool
hanoiTest = and
  [
    [("a","c"), ("a","b"), ("c","b")] == hanoi 2 "a" "b" "c",
    32767 == (length $ hanoi 15 "a" "b" "c")
  ]


-- Return the list of moves to be performed to move n discs from the source
-- peg to the destination peg using two auxiliary pegs.
-- We use the Frame-Stewart algorithm:
--     1. for some k (1 <= k < n), move k discs from src to aux1
--     2. move the remaining n-k discs from src to dst without using aux1
--     3. move k discs from aux1 to dst
-- It has been proved that for 4 pegs, the value of k that minimizes
-- the number of moves is the one that we have used here.
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame%E2%80%93Stewart_algorithm
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n src dst aux1 aux2 = 
  moveFromSrcToAux1 ++ moveFromSrcToDst ++ moveFromAux1ToDst
  where
    moveFromSrcToAux1 = hanoi4 k src aux1 aux2 dst
    moveFromSrcToDst  = hanoi (n-k) src dst aux2
    moveFromAux1ToDst = hanoi4 k aux1 dst aux2 src
    n' = fromIntegral n :: Double
    k  = n - round (sqrt (2*n' + 1)) + 1

hanoi4Test :: Bool
hanoi4Test = and
  [
    [("a","c"), ("a","b"), ("c","b")] == hanoi4 2 "a" "b" "c" "d",
    129 == (length $ hanoi4 15 "a" "b" "c" "d"),
    289 == (length $ hanoi4 20 "a" "b" "c" "d")
  ]
