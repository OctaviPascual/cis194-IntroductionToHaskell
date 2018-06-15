{-# OPTIONS_GHC -Wall -fno-warn-missing-methods -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where


---------------------------------- Exercise 1 ----------------------------------

-- Return the nth Fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Infinte list of all Fibonacci numbers (this is very slow!)
fibs1 :: [Integer]
fibs1 = fib <$> [0..]


fibTest :: Bool
fibTest = and
  [
    0 == fib 0,
    1 == fib 1,
    5 == fib 5,
    89 == fib 11,
    610 == fib 15
  ]

fibs1Test :: Bool
fibs1Test = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987] == take 17 fibs1


---------------------------------- Exercise 2 ----------------------------------

-- Infinte list of all Fibonacci numbers (efficiently computed)
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


fibs2Test :: Bool
fibs2Test = take 25 fibs1 == take 25 fibs2


---------------------------------- Exercise 3 ----------------------------------

data Stream a = Stream a (Stream a)

-- Convert a stream into an infinte list
streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


---------------------------------- Exercise 4 ----------------------------------

-- Generates a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

-- Applies a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

-- Generates a stream from a seed and a rule which specifies how to transform
-- the seed into a new seed which is used for generating the rest of the stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))


streamRepeatTest :: Bool
streamRepeatTest = and
  [
    (show . take 20 $ repeat 'a')       == show (streamRepeat 'a'),
    (show . take 20 $ repeat True)      == show (streamRepeat True),
    (show . take 20 $ repeat "Haskell") == show (streamRepeat "Haskell")
  ]

streamMapTest :: Bool
streamMapTest = and
  [
    (show . take 20 $ repeat 'a')   == (show . streamMap id  $ streamRepeat 'a'),
    (show . take 20 $ repeat False) == (show . streamMap not $ streamRepeat True)
  ]

streamFromSeedTest :: Bool
streamFromSeedTest = and
  [
    (show . take 20 $ repeat 'a')       == show (streamFromSeed id 'a'),
    (show . take 20 $ ([0..] :: [Int])) == show (streamFromSeed (+1) (0 :: Int))
  ]


---------------------------------- Exercise 5 ----------------------------------

-- Infinte list of natural numbers starting from 0
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Generates the ruler function
ruler :: Stream Integer
ruler = foldr1 interleaveStreams (streamRepeat <$> [0..])

-- Alternates the elements from two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)


natsTest :: Bool
natsTest = (show . take 20 $ ([0..] :: [Integer])) == show nats

rulerTest :: Bool
rulerTest = "[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]" == show ruler

interleaveStreamsTest :: Bool
interleaveStreamsTest = (show . take 20 $ cycle [True,False]) == show streams
  where streams = interleaveStreams (streamRepeat True) (streamRepeat False)


---------------------------------- Exercise 6 ----------------------------------

-- Generates the polynomial: 0 + 1*x + 0*x^2 + 0*x^3 + ..
x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (Stream x xs) +   (Stream y ys) = Stream (x+y) (xs + ys)
  (Stream x xs) * s@(Stream y ys) = Stream (x*y) (streamMap (*x) ys + (xs * s))
  negate                          = streamMap negate
  fromInteger n                   = Stream n $ streamRepeat 0

instance Fractional (Stream Integer) where
  (Stream x xs) / (Stream y ys) = q
    where q = Stream (x `div` y) (streamMap (`div` y) (xs - q * ys))

-- Infinte list of all Fibonacci numbers (computed using polynomials)
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)


xTest :: Bool
xTest = (show $ take 20 $ [0,1] ++ zeros) == show x
  where zeros = repeat (0 :: Integer)

instanceNumTest :: Bool
instanceNumTest = and
  [
    (show . take 20 $ [0,2]   ++ zeros) == show (x + x),
    (show . take 20 $ [0,0,1] ++ zeros) == show (x * x),
    (show . take 20 $ [0,-1]  ++ zeros) == show (negate x),
    (show . take 20 $ [5]     ++ zeros) == show fromInteger5
  ]
  where zeros = repeat (0 :: Integer)
        fromInteger5 = (fromInteger 5) :: Stream Integer

instanceFractionalTest :: Bool
instanceFractionalTest = and
  [
    (show . take 20 $ [0,1] ++ zeros) == (show $ x / 1),
    (show . take 20 $ [3,1] ++ zeros) == (show $ (x-3)*(x+3) / (x-3))
  ]
  where zeros = repeat (0 :: Integer)

fibs3Test :: Bool
fibs3Test = (show $ take 20 fibs1) == show fibs3



---------------------------------- Exercise 7 ----------------------------------

data Matrix = Matrix { a00 :: Integer
                     , a10 :: Integer
                     , a01 :: Integer
                     , a11 :: Integer
                     } deriving (Show, Eq)

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

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = a01 (f^n)
  where f = Matrix 1 1
                   1 0


instanceMatrixTest :: Bool
instanceMatrixTest = and
  [
    Matrix 2 4 6 8             == a + a,
    Matrix 7 10 15 22          == a * a,
    Matrix (-1) (-2) (-3) (-4) == -a,
    Matrix 5 5 5 5             == b
  ]
  where a = Matrix 1 2
                   3 4
        b = (fromInteger 5) :: Matrix

fib4Test :: Bool
fib4Test = take 20 fibs1 == (take 20 $ fib4 <$> [0..])
