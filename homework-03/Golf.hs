{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (transpose)


---------------------------------- Exercise 1 ----------------------------------

-- Return every n-th element of a list starting at the first element
skipEvery :: Int -> [a] -> [a]
skipEvery _ []     = []
skipEvery n (x:xs) = x : skipEvery n (drop n xs)

-- Return every n-th element of a list starting at the n-th element
every :: Int -> [a] -> [a]
every n = skipEvery n . drop n

-- Return a list of lists where the n-th list in the output contains every
-- n-th element from the input list
-- For example, the second list in the output contains every second element
-- from the input list
skips :: [a] -> [[a]]
skips xs = [every i xs | i <- [0..n]]
  where n = length xs - 1


skipsTest :: Bool
skipsTest = and
  [
    ["ABCD", "BD", "C", "D"] == skips "ABCD",
    ["hello!", "el!", "l!", "l", "o", "!"] == skips "hello!",
    [[True, False], [False]] == skips [True, False]
  ]


---------------------------------- Exercise 2 ----------------------------------

-- Return all the local maxima in the input list in order
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it
localMaxima :: [Integer] -> [Integer]
localMaxima l@(x:y:z:_)
  | y > x && y > z = y : localMaxima (tail l)
  | otherwise      = localMaxima $ tail l
localMaxima _ = []


localMaximaTest :: Bool
localMaximaTest = and
  [
    [] == localMaxima [1, 2],
    [9, 6] == localMaxima [2, 9, 5, 6, 1],
    [4] == localMaxima [2, 3, 4, 1, 5],
    [] == localMaxima [1, 2, 3, 4, 5]
  ]


---------------------------------- Exercise 3 ----------------------------------

-- Return the number of occurrences of an element in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Return a string of length maxF having f *
bar :: Int -> Int -> String
bar f maxF = take maxF $ replicate f '*' ++ repeat ' '

-- Takes as input a list of integers between 0 and 9 and returns a textual
-- representation of a vertical histogram showing how many of each number
-- were in the input list (use putStr $ histogram [3, 5] to visualize)
histogram :: [Integer] -> String
histogram xs = unlines $ rotate bars ++ legend
  where
    frequencies = [count i xs | i <- [0..9]]
    maxF = maximum frequencies
    bars = [bar f maxF | f <- frequencies]
    rotate = reverse . transpose
    legend = ["==========", "0123456789"]


histogramTest :: Bool
histogramTest = and
  [
    "   * *    " ++ legend == h1,
    " *        \n *        \n *   *    " ++ legend == h2,
    "    *     \n    *     \n    * *   \n ******  *" ++ legend == h3
  ]
  where
    h1 = histogram [3, 5]
    h2 = histogram [1, 1, 1, 5]
    h3 = histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
    legend = "\n==========\n0123456789\n"
