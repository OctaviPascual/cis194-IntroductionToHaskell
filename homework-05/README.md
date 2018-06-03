# Homework 5

This homework contains the following files:

- [Assignment](assignment.pdf)
- [ExprT.hs](ExprT.hs)
- [Parser.hs](Parser.hs)
- [StackVM.hs](StackVM.hs)
- [Calc.hs (file to submit)](Calc.hs)

In this homework we implement a simple calculator using a domain-specific language (DSL). For arithmetic expressions, we are going to work with the following data type `ExprT`:

```haskell
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
```

## Exercise 1

In this exercise we implement a function `eval :: ExprT -> Integer` which takes an expression and evaluates it. This is trivial to do using pattern matching:

```haskell
eval :: ExprT -> Integer
eval (ExprT.Lit x)   = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y
```

## Exercise 2

The function `evalStr :: String -> Maybe Integer` evaluates an expression given as a `String` such as `(2+3)*4`. If the expression is not well-formed, it should return `Nothing` whereas it should return `Just n` if the expression evaluates to `n`. We are provided with a helper function named `parseExp` which does the hard job, that is parsing the `String` to `ExprT`. The only obstacle is that we have to deal with `Maybe` since `parseExp` might not be able to successfully parse the expression.

My first approach was the following:

```haskell
evalStr :: String -> Maybe Integer
evalStr = (maybe Nothing $ Just . eval) . parseExp ExprT.Lit ExprT.Add ExprT.Mul
```

However, I was not satisfied with this solution since I felt that using the `maybe :: b -> (a -> b) -> Maybe a -> b` function just to forward the `Nothing` and to reconstruct the `Just` was too verbose. So I researched and indeed there was a much more elegant solution:

```haskell
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul
```

Actually it took me a while to understand why this worked. Let's take a look at the `fmap :: Functor f => (a -> b) -> f a -> f b` function. It takes a function `a -> b` and applies it to `a` to return a `b`, as long as `a` and `b` are instances of the `Functor f`. The function `eval :: ExprT -> Integer` implies that `fmap eval :: Functor f => f ExprT -> f Integer`. So it is a function that takes an `ExprT` and returns an `Integer`. But those two elements must be an instance of the `Functor` type class! Well, in fact `Maybe` type is an instance of `Functor`. For example, `fmap (*2) Nothing` is equal to `Nothing` while `fmap (*2) $ Just 5` is equal to `Just 10`. So, in the same way we can apply `fmap` to `[a]`, we can apply `fmap` to `Maybe`.

## Exercise 3

We had to create a type class named `Expr` to parallel the constructors of `ExprT`. We see that the types of `ExprT` constructors are the following:
* `Lit :: Integer -> ExprT`
* `Add :: ExprT -> ExprT -> ExprT`
* `Mul :: ExprT -> ExprT -> ExprT`

Thus, the `Expr` is defined as follows:

```haskell
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
```

Then we have to make `ExprT` an instance of `Expr`. This is also straightforward:

```haskell
instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul
```

Finally, note that an expression such as `lit 5` has type `Expr a => a`, that is any type which is instance of `Expr` type class. If we want to give an explicit type to such an expression, we can explicitly do it like this `lit 5 :: ExprT` or we can use an expression which context helps determining the type. A trivial function that does this is the following one:

```haskell
reify :: ExprT -> ExprT
reify = id
```

Now the expression `reify $ lit 5` has type `ExprT`, since the function `reify` takes an `ExprT` as first argument.

## Exercise 4

Here we had to keep making instances of `Expr`. The first two ones are `Integer` and `Bool`. So we have to define how they will implement the type class `Expr`. We can use point-free notation and sections to simplify the code. Note that we use the `id :: a -> a` function since the signature of lit is `lit :: Integer -> a`, so in the `Integer` instance signature is `lit :: Integer -> Integer`.

```haskell
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (<= 0)
  add = (||)
  mul = (&&)
```

The following two instances are `MinMax` and `Mod7`. Here we also define wrappers with `newType` to work with `Integers` internally. Here we cannot be as concise as the previous ones as we have to pattern match the `MinMax` and `Mod7` types.

```haskell
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x                 = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x+y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x*y) `mod` 7
```

Finally, this led me to wonder what was the difference between `data` and `newtype` when defining new data structures. Basically `newtype` is limited to a single constructor. For example, we cannot use algebraic data types with `newtype`. Meanwhile, `data` declares a new data structure at runtime. For more info, read [this](https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell).

## Exercise 5

I think the complexity of this exercise resided in understanding what we had to do. We have to implement another calculator using a custom stack-based CPU. I highly recommend reading and understanding the statement and taking a look at [StackVM.hs](StackVM.hs) file. I took the license of changing the file a little bit, such as adding the `Eq` type class to both `StackVal` and `StackExp` types in order to be able to test the functions.

The task is to implement a compiler for arithmetic expressions only. That means that we are not going to use some operations that the custom CPU supports such as boolean operations. First we have to create an instance of `Expr` type class for `Program` which is simply a list of `StackExp`. For example, `[PushI 3, PushI 5, Add]` is a program. We have to implement `lit`, `add` and `mul` functions:

* `lit :: Integer -> Program`: We just have to push the `Integer` to the stack
* `add :: Program -> Program -> Program`: We have to append the first program, the second program and the `add` operation
* `mul :: Program -> Program -> Program`: We have to append the first program, the second program and the `mul` operation

```haskell
instance Expr Program where
  lit a   = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]
```

Let's take a closer look at what we did here. Suppose we have the following expression: `add (lit 5) (lit 7) :: Program`. The correct program is `[PushI 5,PushI 7,Add]` and indeed the instance we just defined does that.

Then we had to create a function `compile :: String -> Maybe Program` which takes an arithmetic expression and transforms it into a `Program` ready to be run on the custom CPU. To do that we will use again the helper function `parseExp :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a`. The signature of `compile` forces the type variable `a` to be bound `Program`, so the function is as simple as follows:

```haskell
compile :: String -> Maybe Program
compile = parseExp lit add mul
```

Finally, I wanted to try that indeed the `compile` function produced a program that was executable by the custom CPU, so I implemented the function `run :: String -> Either String StackVal` which compiles and executes an arithmetic expression:

```haskell
run :: String -> Either String StackVal
run = execute . compile
  where execute Nothing  = Left "The program does not compile."
        execute (Just p) = stackVM p
```

## Exercise 6

The last exercise was the most challenging one, but also the one that made me learn the most! The objective of this exercise was to allow variables in our arithmetic expressions.

First of all, we had to create a new type class named `HasVars` with the method `var :: String -> a`. Then a new data type `VarExprT`, similar to `ExprT` but with an additional constructor for variables. For now, nothing too fancy:

```haskell
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)
```

Next, we had to make `VarExprT` an instance of both `Expr` and `HasVars`. We have already done this previously, so again, this is straightforward:

```haskell
instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Calc.Var
```

Now, we are done with `VarExprT` and the interesting part begins! We will use the `Data.Map` module to deal with maps since we need to store mappings from variables to values. If this is the first time you are dealing with maps, it is worth checking [here](http://learnyouahaskell.com/modules#data-map) how they work. We will use a map from `String` to `Integer` and define the following type for convenience: `type MapSI = M.Map String Integer`.

We must implement two instances: `instance HasVars (MapSI -> Maybe Integer)` and `instance Expr (MapSI -> Maybe Integer)`. I will just drop the code here, which is pretty concise, and then I will explain what happens under the hoods:

```haskell
instance HasVars (MapSI -> Maybe Integer) where
  var = M.lookup

instance Expr (MapSI -> Maybe Integer) where
  lit a _   = Just a
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m
```

So... it seems pretty easy right? Well, it took me a while to get there. The first remarkable thing is that `(MapSI -> Maybe Integer)` is not a regular data type such as `VarExprT`, it is a function. So yes, we are implementing two instances of a function. When we say we Haskell has higher-order functions, we take it very seriously!

For the `HasVars` type class we have to implement `var :: String -> MapSI -> Maybe Integer`. If we take a look at `M.lookup :: Ord k => k -> M.Map k a -> Maybe a` we see that it has exactly the same signature, so it is as easy as `var = M.lookup`.

For the `Expr` type class I had a lot of troubles understanding how `add` and `mul` signatures worked. They are `a -> a -> a` thus `(MapSI -> Maybe Integer) -> (MapSI -> Maybe Integer) -> (MapSI -> Maybe Integer)`. But in Haskell this is equivalent to `(MapSI -> Maybe Integer) -> (MapSI -> Maybe Integer) -> MapSI -> Maybe Integer`, since `->` is right associative. That means that when we do `add a b m`, `m` is bound to `MapSI` and not to `(MapSI -> Maybe Integer)`. Similarly, for the `lit` function the signature is `Integer -> (MapSI -> Maybe Integer)`. By the way, when we write `a m` and `b m` we are working with the types `a :: (MapSI -> Maybe Integer)` and `m :: MapSI`. So if we do `a m` we have the signature: `(MapSI -> Maybe Integer) -> MapSI -> Maybe Integer`. Taking all of that into account, this was my first approach:

```haskell
instance Expr (MapSI -> Maybe Integer) where
  lit a     = (\_ -> Just a)
  add a b m = case (isNothing (a m) || isNothing (b m)) of
                True -> Nothing
                _    -> Just (fromJust (a m) + fromJust (b m))
  mul a b m = case (isNothing (a m) || isNothing (b m)) of
                True -> Nothing
                _    -> Just (fromJust (a m) * fromJust (b m))
```

As you can see, it is pretty verbose and tedious to extract the values from `Maybe` only to wrap them again in `add` and `mul` while the `lit` function is also strange. For the latter, it is just as easy as realising that `Integer -> (MapSI -> Maybe Integer)` is the same as `Integer -> MapSI -> Maybe Integer` so `lit a _   = Just a` makes more sense. For the former, I was lucky enough to find a nice explanation of what [applicative style](https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Application_in_functors) is. It is based on the problem of suming `Just 2` and `Just 3` so it is perfectly suited for this example.

Finally, I take the license to show again my final version, just in case you missed it:

```haskell
instance HasVars (MapSI -> Maybe Integer) where
  var = M.lookup

instance Expr (MapSI -> Maybe Integer) where
  lit a _   = Just a
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m
```
