# Homework 4

This homework contains the following files:

- [Assignment](assignment.pdf)
- [HigherOrder.hs (file to submit)](HigherOrder.hs)

Next I try to explain how I solved each exercise.

## Exercise 1

In this exercise we had to reimplement functions using *wholemeal programming* practices.

The first function is `fun1 :: [Integer] -> Integer` which takes a list of integers and returns a single one. This is pretty straightforward to rewrite since we only need to filter even integers, subtract 2 to each of them, and then compute their product.

```haskell
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even
```

The second function is `fun2 :: Integer -> Integer` which computes the hailstone sequence of a number and sums the even numbers of the sequence. We use `iterate :: (a -> a) -> a -> [a]` to produce the whole sequence and `takeWhile :: (a -> Bool) -> [a] -> [a]` to take the values that are greatest than 1 (when 1 is reached, the hailstone sequence ends). Then we just have to filter even numbers and compute their sum.

```haskell
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate hailstone
  where hailstone n = if even n then n `div` 2 else 3*n + 1
```

## Exercise 2

Here we had to implement a function `foldTree :: [a] -> Tree a` which generates a balanced binary tree from a list of values using `foldr`. To represent binary trees we use the following data type, where `Integer` is the height at that node (height is defined as the length of a path from the root to the deepest node):

```haskell
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
```

This is similar to one exercise that we completed in a [previous homework](../homework-02/README.md#exercise-3). The idea is to create a function which inserts a node into a balanced tree and returns a new balanced tree containing that node. If we have that function, we can just generate the whole tree like this:

```haskell
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
```

The hard part is to implement `insert :: a -> Tree a -> Tree a` function. Note that we can always maintain the tree balanced by replacing a `Leaf` by the node that we are trying to insert. That means that we won't need rebalancing the tree, so any node that is already in the tree will never be moved. However, we might have to update the height of the nodes.

The base case is easy: if the tree is a `Leaf`, we just return a new node with height 0. If the tree is not a leaf, we must decide whether we want to insert the node into the left subtree or into the right subtree. To make that decision we can check the heights of the left and the right subtrees. Three cases may happen:
* If the left subtree's height is smaller than the right one, then we insert the node into the left subtree. Note that the height of the current node does not need an update since the length of the path to the deepest node of the left subtree will not overcome the one of the right subtree.
* If the right subtree's height is smaller than the left one, we proceed exactly as above, only swap left by right and right by left. Indeed, those two cases are symmetrical.
* Finally, if both heights are equal, we arbitrarily chose to insert the node into the left subtree. However, here the height of the current node might change. How much can it change? Well, the new height of the node will be the height of the left subtree with the new element inserted (`left'`) plus one. This is because `left'`s height will be the length of the path to the deepest node, and the current node is just one position further from that node than `left'`. 

```haskell
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left root right)
  | h1 < h2   = Node h (insert x left) root right
  | h1 > h2   = Node h left root (insert x right)
  | otherwise = Node (h3+1) left' root right
  where h1 = height left
        h2 = height right
        h3 = height left'
        left' = insert x left
```

Finally, note that we have defined a helper `height :: Tree a -> Integer` function which returns the height of a tree. This function is trivial as we already store the height in the node but has the particularity that we return -1 instead of 0 if the tree is a `Leaf`. We must do that in order to distinguish between a node with height 0 and a Leaf. That way, when the left subtree is a node with height 0 and the right one is a `Leaf`, we will insert the node into the right subtree and not into the left one since 0 > -1.

```haskell
height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h
```

## Exercise 3

The function `xor :: [Bool] -> Bool` returns `True` if and only if there are an odd number of `True`
values contained in the input list. We must use a fold function. Since xor logical operation is associative, we can just use xor as the function to fold with. It is funny how, in Haskell, xor can be implemented the following way:

```haskell
xor :: Bool -> Bool -> Bool
xor = (/=)
```

In this case, it does not matter whether we use `foldl` or `foldr`. The final solution is:

```haskell
xor :: [Bool] -> Bool
xor = foldr (/=) False
```

The function `map' :: (a -> b) -> [a] -> [b]` must act exactly as the standard `map` function, but we must use `foldr`. Let's remember its signature: `foldr :: (a -> b -> b) -> b -> [a] -> b`. If we want to implement map, we realise that the type `b` that `foldr` returns must be the list `[b]` that `map'` returns. So in our case the signature is `foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]`. Thus, we accumulate the results in a list. The function that we feed `foldr` with takes an element of type `a` and a list `[b]` and returns a list `[b]`. To act as `map`, we just have to take the element `a`, apply the mapping function `f` to that element to obtain a `b`, and add it to the list of `[b]`.

My first implementation was:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
```

But probably a more elegant (and probably harder to understand) solution is:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
```

Finally, the function `myFoldl :: (a -> b -> a) -> a -> [b] -> a` must behave identically to the standard `foldl` function and must use `foldr`. If we take a look at both signatures, were we have renamed the types to easily spot where the differences are: `foldl :: (a -> b -> a) -> a -> [b] -> a` and `foldr :: (b -> a -> a) -> a -> [b] -> a`. We see that we just have to transform the `(a -> b -> a)` function into `(b -> a -> a)`, since that's what `foldr` needs. The `flip :: (a -> b -> c) -> b -> a -> c` function does exactly what we need. Actually that looked too easy, so I [researched](https://wiki.haskell.org/Foldl_as_foldr) to check whether or not it was correct and I couldn't find my answer. So probably this solution fails for some cases but I haven't been able to find them.

```haskell
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = foldr . flip
```

## Exercise 4

In this exercise we implemented the [sieve of Sundaram](https://en.wikipedia.org/wiki/Sieve_of_Sundaram) which is an algorithm for finding all the prime numbers up to a specific integer. The algorithm is somewhat similar to the sieve of Eratosthenes in the sense that we will also cross out numbers. The function `sieveSundaram :: Integer -> [Integer]` generates all the odd prime numbers up to 2*n* + 2.

First of all we have to generate a list of integers from 1 to n, in Haskell `[1..n]`. Then we must cross out (remove) from this list all numbers of the form *i* + *j* + 2*ij* where 1 <= *i* <= *j* and *i* + *j* + 2*ij* <= *n*. In order to remove elements from a list, we can use the difference operator: `(\\) :: Eq a => [a] -> [a] -> [a]`.

To generate all the numbers to cross out, we can use comprehension lists. Indeed, the algorithm that we defined above perfectly fits for that: `[x | i <- [1..n], j <- [i..n], let x = i + j + 2*i*j, x <= n]`. Yes, this is valid Haskell code, maybe it is even easier to understand than the explanation in simple words!

Finally, we have to work with the numbers that are left in the `[1..n]` list. They must be doubled and incremented by one, and that is the list of odd prime numbers below 2*n* + 2. We just have to map the lambda function `\x -> 2*x + 1` to those numbers and that's all. Note that we also could have used function composition like this `((+1) . (*2))`, but I feel it is less readable than the lambda function. Instead of using `map` I used its infix notation `<$>`.

```haskell
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (\x -> 2*x + 1) <$> [1..n] \\ crossOut
  where crossOut = [x | i <- [1..n], j <- [i..n], let x = i + j + 2*i*j, x <= n]
```
