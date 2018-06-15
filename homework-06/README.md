# Homework 6

This homework contains the following files:
* [Assignment](assignment.pdf)
* [Fibonacci.hs (file to submit)](Fibonacci.hs)

In this homework we work with [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

## Exercise 1

Usually, the first approach we take when dealing with Fibonacci numbers is to translate its definition directly into a recursive (and very inefficient) function, and this homework is not an exception. The function `fib :: Integer -> Integer` returns the *n*-th Fibonacci number:

```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

The function `fibs1 :: [Integer]` generates the infinite list of Fibonacci numbers:

```haskell
fibs1 :: [Integer]
fibs1 = fib <$> [0..]
```

By the way, by evaluating `fibs1` on the GHCi you can actually see how slow `fib` is. Usually, when printing an infinite list you quickly interrupt the execution. On the contrary, here you have plenty of time to do it!

## Exercise 2

Obviously the next step to take is to implement a more efficient solution. The function `fibs2 :: [Integer]` generates the same list as `fibs1` but it requires only *O(n)* operations to compute the first *n* elements. By taking advantage of Haskell's lazy evaluation, we can come up with the following solution:

```haskell
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
```

`fibs2` starts being `[0,1]` and to obtain the following element we call `fibs2` recursively, and we do it twice. Let's think about how the third element is generated. Initially, `fibs2` is `[0,1]` and `tail fibs2` is `[1]`, so we can already start computing the `zipWith` function with those two lists to generate `0+1 = 1`. Now, `fibs2` is the list `[0,1,1]`. `zipWith` continues with `[0,1,1]` and `[0,1]` to generate `[0,1,1,2]`, and so on.

In summary, this definition can be read as: start with `[0,1]` and, to generate the rest of the Fibonacci sequence, take the Fibonacci sequence up to now (which represents F<sub>*n-1*</sub>) and sum it with the Fibonacci sequence up to now shifted one position to the left (which represents F<sub>*n-2*</sub>).

Probably this is not the best explanation ever, so visit [this](https://stackoverflow.com/questions/6273621/understanding-a-recursively-defined-list-fibs-in-terms-of-zipwith) if you want a more detailed explanation.

## Exercise 3

We are asked to design a polymorphic data type named `Stream`. It represents an infinite list, so a stream can be seen as an element followed by a stream.

First we must create the `Stream` data type. In Haskell we can define recursive data structures, so it is as easy as that:

```haskell
data Stream a = Stream a (Stream a)
```

Note that the `Stream` right after the equals is the value constructor (in green), which can be different from the data type, but in this case I couldn't figure a better name.

The function `streamToList :: Stream a -> [a]` converts a stream into an infinite list:

```haskell
streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs
```

Finally, we must make `Stream` instance of `Show` type class in order to visualize it. But how can we visualize an infinite structure? Well, we can just print a prefix of the stream, for example its 20 first elements. For convenience, we transform the stream into a list:

```haskell
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList
```

## Exercise 4

Now we can start working with streams. To do that, in the first place we must create a function that generates a stream. The function `streamRepeat :: a -> Stream a` generates a stream containing infinitely many copies of the given element:

```haskell
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)
```

The function `streamMap :: (a -> b) -> Stream a -> Stream b` applies a function to every element of the stream, acting like `map :: (a -> b) -> [a] -> [b]` for lists:

```haskell
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)
```

The function `streamFromSeed :: (a -> a) -> a -> Stream a` generates a stream from a seed and a rule which specifies how to transform the seed into a new seed which is used for generating the rest of the stream. In other words, this is similar to the `iterate :: (a -> a) -> a -> [a]` function for lists:

```haskell
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))
```

## Exercise 5

The function `nats :: Stream Integer` generates an infinite list of natural numbers starting from 0:

```haskell
nats :: Stream Integer
nats = streamFromSeed (+1) 0
```

The function `ruler :: Stream Integer` generates the [ruler function](http://oeis.org/A007814). The real challenge was to do it without using any kind of divisibility testing and I ended up searching how to do it. I managed to find an [answer](https://codereview.stackexchange.com/a/66811) which is worth reading.

First of all we define a function `interleaveStreams :: Stream a -> Stream a -> Stream a` which alternates the elements from two streams. My first implementation was the following:

```haskell
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) (Stream y ys) = Stream x (Stream y $ interleaveStreams xs ys)
```

I tried it and it worked, so I moved on to the `ruler :: Stream Integer` function. But when I implemented `ruler` the following way, it never ended (read the [answer](https://codereview.stackexchange.com/a/66811) I previously mentioned to why this works):

```haskell
ruler :: Stream Integer
ruler = foldr1 interleaveStreams1 (streamRepeat <$> [0..])
```

I thought that the problem was in the `foldr1` (which is just a `foldr` that uses the last element of the list as initial element) but in reality the problem ended up being in `interleaveStreams`. The problem is that with the above implementation, both streams are evaluated which breaks Haskell's lazyness. We can reimplement `interleaveStreams` by only evaluating the element of the first stream, and we can just get the following element later:

```haskell
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)
```

With that change, `ruler` works as intended! Indeed, the `foldr1` will look like this: `interleave (streamRepeat 0) (interleave (streamRepeat 1) (interleave (streamRepeat 2) ...))`. Then, `interleave (streamRepeat 0) X` can be evaluated since `X` will just be a [thunk](https://wiki.haskell.org/Thunk), so we can continue with our computation. Next, we can evaluate `interleave (streamRepeat 1) Y` and so on.

It is definitely worth it to stop here and fully understand this exercise, it took me a while to do it but it made me learn a lot.

## Exercise 6

The idea of this exercise is to work with polynomials. We will store the coefficients in a `Stream Integer`.

The function `x :: Stream Integer` returns the monomial of degree 1 and coefficient 1, that is, *x*. Since the function is named `x`, I had to add the `-fno-warn-name-shadowing` to turn off `-Wname-shadowing` warning. To generate the coefficients of `x`, we just have to start with zero, then one, and fill the rest of the stream with zeros:

```haskell
x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0
```

Next we had to make `Stream Integer` an instance of `Num` type class. For that, we must implement the functions that `Num` defines. We can take a look at what those functions look like:

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

Since we will not implement all of them, we will add the `-fno-warn-missing-methods` at the beginning of the file. The implementation is the following:

```haskell
instance Num (Stream Integer) where
  (Stream x xs) +   (Stream y ys) = Stream (x+y) (xs + ys)
  (Stream x xs) * s@(Stream y ys) = Stream (x*y) (streamMap (*x) ys + (xs * s))
  negate                          = streamMap negate
  fromInteger n                   = Stream n $ streamRepeat 0
```

Note that when we define `(+)`, we use both the `(+) :: Integer -> Integer -> Integer` and the `(+) :: Stream Integer -> Stream Integer -> Stream Integer` functions. The `x+y` corresponds to the first one whereas the `(xs + ys)` to the second one. Thus, we define `(+)` recursively. The same case happens for `(*)`. Finally, note that the signature of the `negate` that is on the left side is `negate :: Stream Integer -> Stream Integer` while the one that is on the right side is `negate :: Integer -> Integer`.

The next step is to declare `Stream Integer` an instance of `Fractional` in order to define how `(/)` works for polynomials. We just have to implement the formula that is given to us (note that *q* variable is defined recursively):

```haskell
instance Fractional (Stream Integer) where
  (Stream x xs) / (Stream y ys) = q
    where q = Stream (x `div` y) (streamMap (`div` y) (xs - q * ys))
```

Finally, with the previous operations we have defined for polynomials, we can define Fibonacci sequence the following way:

```haskell
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)
```

## Exercise 7

In the last exercise we create a `Matrix` data type in order to represent 2 x 2 matrices of integers. We will use matrix binary exponentiation to compute the *n*th Fibonacci number in *O*(log n).

First of all we define the `Matrix` data type using record syntax in order to easily lookup its fields:

```haskell
data Matrix = Matrix { a00 :: Integer
                     , a10 :: Integer
                     , a01 :: Integer
                     , a11 :: Integer
                     } deriving (Show, Eq)
```

Next we must make `Matrix` an instance of `Num` which is easy for 2 x 2 matrices:

```haskell
instance Num Matrix where
  (Matrix a00 a10 a01 a11) + (Matrix a00' a10' a01' a11') =
    Matrix (a00 + a00') (a10 + a10')
           (a01 + a01') (a11 + a11')
  (Matrix a00 a10 a01 a11) * (Matrix a00' a10' a01' a11') =
    Matrix (a00*a00' + a10*a01') (a00*a10' + a10*a11')
           (a01*a00' + a11*a01') (a01*a10' + a11*a11')
  negate (Matrix a00 a10 a01 a11) =
    Matrix (-a00) (-a10)
           (-a01) (-a11)
  fromInteger n = Matrix n n n n
```

Finally we can compute the *n*th Fibonacci number just by raising a matrix:

```haskell
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = a01 (f^n)
  where f = Matrix 1 1
                   1 0
```
