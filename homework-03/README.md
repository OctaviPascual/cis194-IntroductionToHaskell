# Homework 3

This homework contains the following files:

- [Assignment](assignment.pdf)
- [Golf.hs (file to submit)](Golf.hs)

Next I try to explain how I solved each exercise.

## Exercise 1

The function `skips :: [a] -> [[a]]` returns a list of lists where the *n*th list in the output contains every n*th* element from the input.

First we must implement a helper function that, given a list, returns a list of its *n*th elements. This is what `skipEvery :: Int -> [a] -> [a]` does. This function skips every *n*th element of a list starting at the first element.

```haskell
skipEvery :: Int -> [a] -> [a]
skipEvery _ []     = []
skipEvery n (x:xs) = x : skipEvery n (drop n xs)
```

The problem of `skipEvery` is that no matter what the value of n is, it will include the first element of the list. In `skips` we don't start at the begining of the list but at the *n*th element. That's why we have defined `every :: Int -> [a] -> [a]` function that drops the first n elements of the list before calling `skipEvery`. Doing that we start at the *n*th element. We have defined `every` function using [pointfree style](https://wiki.haskell.org/Pointfree).

```haskell
every :: Int -> [a] -> [a]
every n = skipEvery n . drop n
```

Once we have `every` function, we just have to call it `n` times with the right parameters, where `n` is the length of the input list. First, we must take every 0 elements, as we want the same list as the input. Second, we want to take every 1 element, and so on. That corresponds to the following calls: `every 0 xs`, `every 1 xs` and so on. To accomplish that we can use `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]` function.

```haskell
skips :: [a] -> [[a]]
skips [] = []
skips xs = zipWith every [0..n] $ repeat xs
  where n = length xs - 1
```

Note that we can use `repeat`, which generates an infinite sequence, since `zipWith` discards elements of the longer list. I think this is more elegant than doing `replicate (length xs) xs`.

Finally, I realised that there was a much simpler way to implement `skips`. We can use comprehension lists to avoid having to use `zipWith` and replicating the list.

```haskell
skips :: [a] -> [[a]]
skips xs = [every i xs | i <- [0..n]]
  where n = length xs - 1
```

## Exercise 2

The function `localMaxima :: [Integer] -> [Integer]` must return all the local maxima of the input list in the same order as they appear. A *local maximum* of a list is an element of the list which is strictly greater than both the elements immediately before and after it.

To implement this function we just need to take advantage of pattern matching. We can take the first three elements of the list, named `x`, `y` and `z`, and check whether or not `y` is a local maxmimum. If yes, just prepend this element to the resulting list, which is obtained by calling `localMaxima`recursively. If not, we just call `localMaxima` without prepending any element.

```haskell
localMaxima :: [Integer] -> [Integer]
localMaxima l@(x:y:z:_)
  | y > x && y > z = y : localMaxima (tail l)
  | otherwise      = localMaxima $ tail l
localMaxima _ = []
```

Note that we call `localMaxima` without `x` to avoid infinite recursion and that `tail` will never be called on an empty list `l` as we know that it contains at least two elements, `y` and `z`.

## Exercise 3

The function `histogram :: [Integer] -> String` takes a list of integers between 0 and 9 and outputs a vertical histogram showing how many of each number were in the input list.

First of all, we need a way of counting how many occurrences of integers between 0 and 9 appear in the list. The function `count :: Eq a => a -> [a] -> Int` does that job. Then we can call this function for each integer: `frequencies = [count i xs | i <- [0..9]]`.

```haskell
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
```

Once we know the frequency of each number, we must print as many stars as occurrences that number has. Note that when we do not print a star, we must print a blank space. The first problem we encounter is that we must print the stars vertically, so each line must take into account the results of the ten numbers at the same time, which is not very convenient for us.

For now, let's think of a similar problem that would be easier. Imagine that, instead of printing the histogram vertically, we should print it horizontally. Something like this:

```text
0=** 
1=*  
2=***
3=   
4=*  
5=   
6=*  
7=** 
8=** 
9=** 
```

It is easy to generate each bar in this case. We just have to print as many stars as occurrences and fill the rest of the bar with blanks, until the bar has the same length as the maximum frequency. In order to generate that bar we use `bar :: Int -> Int -> String` function.

```haskell
bar :: Int -> Int -> String
bar f maxF = take maxF $ replicate f '*' ++ repeat ' '
```

To obtain those bars vertically instead of horizontally, we just have to rotate 90 degrees counterclockwise the horizontal bars. If you aren't convinced, take a look at the following two histograms, where legends have been removed. The second one to corresponds to a 90 degree counterclockwise rotation of the first one.

```text
** 
*  
***
   
*  
   
*  
** 
** 
** 
```

```text
  *       
* *    ***
*** * ****
```

To perform that rotation we can represent the bars as a list of lists (a matrix), where the first element of the list is the bar corresponding to the number 0, the second to the number 1, and so on. Now we can use the `transpose` function followed by the `reverse` to rotate 90 degrees counterclockwise the horizontal bars. That might sound like magic but try it and you will see that indeed it works. So now, after this rotation, we already have vertical bars and we just have to add the legend to obtain the full histogram.

```haskell
histogram :: [Integer] -> String
histogram xs = unlines $ rotate bars ++ legend
  where
    frequencies = [count i xs | i <- [0..9]]
    maxF = maximum frequencies
    bars = [bar f maxF | f <- frequencies]
    rotate = reverse . transpose
    legend = ["==========", "0123456789"]
```

Remember that if you want to visualize the histogram you should use `putStr` function, for example `putStr histogram [3,5]`.
