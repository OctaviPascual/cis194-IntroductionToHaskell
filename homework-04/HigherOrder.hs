{-# OPTIONS_GHC -Wall #-}

module HigherOrder where

import Data.List ((\\))


---------------------------------- Exercise 1 ----------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate hailstone
  where hailstone n = if even n then n `div` 2 else 3*n + 1


fun1'Test :: Bool
fun1'Test = and
  [
    fun1 [] == fun1' [],
    fun1 [1,2,3] == fun1' [1,2,3],
    fun1 [1..100] == fun1' [1..100]
  ]

fun2'Test :: Bool
fun2'Test = and [fun2 i == fun2' i | i <- [1..1000]]


---------------------------------- Exercise 2 ----------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Generate a balanced binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Return the height of a tree
-- Note that since the height of a tree with a single node is defined as 0, we
-- define the height of a Leaf as -1 to be able to distinguish those two cases
height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

-- Insert a new node into an existing balanced binary tree
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left root right)
  | h1 < h2   = Node h (insert x left) root right
  | h1 > h2   = Node h left root (insert x right)
  | otherwise = Node (h3 + 1) left' root right
  where h1 = height left
        h2 = height right
        h3 = height left'
        left' = insert x left


foldTreeTest :: Bool
foldTreeTest = and
  [
    Leaf == foldTree "",
    Node 0 Leaf 'A' Leaf == foldTree "A",
    Node 1 (Node 0 Leaf 'B' Leaf) 'C' (Node 0 Leaf 'A' Leaf) == foldTree "ABC",
    Node 3
      (Node 2
        (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
      'I'
        (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
    'J'
      (Node 2
        (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
      'H'
        (Node 0 Leaf 'C' Leaf)) == foldTree "ABCDEFGHIJ"
  ]


---------------------------------- Exercise 3 ----------------------------------

-- Returns True if and only if there are an odd number of True values
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- Behaves identically to the standard map function
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Behaves identically to the standard foldl function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = foldr . flip


xorTest :: Bool
xorTest = and
  [
    True == xor [True],
    False == xor [False],
    True == xor [False, True, False],
    False == xor [False, True, False, False, True]
  ]

map'Test :: Bool
map'Test = and
  [
    map (&& True) [True, False] == map' (&& True) [True, False],
    map (++ "!") ["hello", "world"] == map' (++ "!") ["hello", "world"]
  ]

myFoldlTest :: Bool
myFoldlTest = and
  [
    foldl (*) 1 [1..10] == myFoldl (*) (1 :: Int) [1..10],
    foldl (-) 0 [1..100] == myFoldl (-) (0 :: Int) [1..100],
    foldl (+) 1 [1..100] == myFoldl (+) (1 :: Int) [1..100],
    foldl (||) False [False,True] == myFoldl (||) False [False,True]
  ]


---------------------------------- Exercise 4 ----------------------------------

-- Generate all the odd prime numbers up to 2*n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (\x -> 2*x + 1) <$> [1..n] \\ crossOut
  where crossOut = [x | i <- [1..n], j <- [i..n], let x = i + j + 2*i*j, x <= n]


sieveSundaramTest :: Bool
sieveSundaramTest = and
  [
    [3] == sieveSundaram 1,
    [3,5,7,11,13,17,19,23,29] == sieveSundaram 14,
    [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71] == sieveSundaram 35
  ]
