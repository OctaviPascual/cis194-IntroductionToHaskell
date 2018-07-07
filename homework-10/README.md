# Homework 10

This homework contains the following files:
* [Assignment](assignment.pdf)
* [AParser.hs (file to submit)](AParser.hs)

In this assignment we work with [parsers](https://en.wikipedia.org/wiki/Parsing). We represent a parser the following way:

```haskell
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
```

Note that the field `runParser` is a function, which may be surprising at first. In previous exercises, fields where things like `String` or `Int`. Its type signature is `runParser :: Parser a -> String -> Maybe (a, String)` which means that given a parser it returns a function instead of a value.


## Exercise 1

In this exercise we start by implementing the function `first :: (a -> b) -> (a, c) -> (b, c)`. What I love about this function is that the assignment doesn't tell us what it should do, but from the type signature we can already figure out how it should work: it takes a function `a -> b`, a tuple `(a, c)` and returns a new tuple with the function applied to its first component. It doesn't leave a room for alternative implementations.

```haskell
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
```

Now, we have to make `Parser` an instance of `Functor` type class. I couldn't figure out how to do it and I ended up looking at [this solution](https://github.com/bschwb/cis194-solutions/blob/master/10-applicative-functors-part1/AParser.hs).

```haskell
instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p
```

Since I couldn't figure out the solution by myself, at least I made the effort to understand it.

Let's start with the type signature of the function we must implement: `fmap :: (a -> b) -> Parser a -> Parser b`. In `fmap f p`, we bind `f` to `a -> b` and `p` to `Parser a` with field `runParser` of type `String -> Maybe (a, String)`. Now we have to return a `Parser b`, in other words, a parser with field `runParser` of type `String -> Maybe (b, String)`. So we only have to transform `Maybe (a, String)` into `Maybe (b, String)`.

The signature of `first` is `(a -> b) -> (a, c) -> (b, c)` so we have `first f :: (a, c) -> (b, c)` which is a function that takes a tuple and returns a new one, with another first component. As it is, this function doesn't help that much. However, it it was `Maybe (a, c) -> Maybe (b, c)` it would be different. If we partially apply `fmap` to `first f`, we accomplish that: `fmap (first f) :: Maybe (a, c) -> Maybe (b, c)`.

Finally, by doing `runParser p` we obtain `String -> Maybe (a, String)`. We have all the functions we need and we just need to glue them together. To do that, we use function composition: we take the output of `runParser p` and we feed it into `fmap (first f)`. So the final solution is: `fmap (first f) . runParser p`.

I don't know if this exercise was indeed complicated or I got stuck, but usually the first exercise is simple and this one took me a while...

## Exercise 2

### Initial version

In the second exercise we implement an `Applicative` instance for `Parser`. The functions we have to implement are `pure :: a -> f a` and `(<*>) :: f (a -> b) -> f a -> f b` where `f` is a `Functor`. In the previous exercise we made `Parser` an instance of `Functor`, so we are ready to start!

Let's begin with the easiest function, which is `pure :: a -> Parser a`. This parser consumes no input and successfully produces a result of `a`:

```haskell
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
```

Nothing too fancy here: since we always succeed, we return a `Just` producing the result of `a` and we leave the input untouched.

Now comes the hard part. It was kinda frustrating needing to take a look again at a [solution](https://github.com/bschwb/cis194-solutions/blob/master/10-applicative-functors-part1/AParser.hs). At least this time I only took a quick look and wrote the function by myself.

```haskell
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  p1 <*> p2 = Parser p
    where p str = case runParser p1 str of
                    Nothing     -> Nothing
                    Just (f, s) -> runParser (f <$> p2) s
```

Again, let's make sure that we fully understand the solution. First, the signature is `(<*>) :: Parser (a -> b) -> Parser a -> Parser b` which means that the first parser produces a function. I had to read many times the assignment to understand that! Probably it is clearer to look at the definition of a parser that produces a function: `Parser (a -> b) = Parser { runParser :: String -> Maybe ((a -> b), String) }`. It is just a regular parser that wraps a function, not that complicated at the end of the day!

The second parameter is `Parser a = Parser { runParser :: String -> Maybe (a, String) }` and we want to apply the function `a -> b` to `a` in order to return the parser `Parser b = Parser { runParser :: String -> Maybe (b, String) }`. By the way, if either p1 or p2 fail, `p1 <*> p2` also should fail.

Now we look at the implementation. First we define the function `p :: String -> Maybe (b, String)` which is the one that will have the parser we return. For convenience, we put the first argument `str` on the left. Then we run p1, which can either fail or consume some input and produce a function. If it fails, we just return `Nothing`. If not, we bind the function it produced to `f` and the remaining input to `s`. If we do `runParser p2 s` we get `Maybe (a, String)`.

The final step is to take that `Maybe (a, String)` and transform it into `Maybe (b, String)`. Well, remember previous exercise? We can use the fact that `Parser` is a `Functor` to apply `fmap` to the parser. We call `runParser :: Parser a -> String -> Maybe (a, String)` with `f <$> p2` as first argument and `s` as second. That way, we run the transformed parser with the remaining input of the first one.

### Improved solution

Even if the initial solution is already pretty clean, I wanted to get rid of the case clause. I was kinda convinced it was possible and thanks to that I learned how to use the operator `(>>=) :: Monad m => m a -> (a -> m b) -> m b` (Actually shame on me, because this was explained in the IO lecture, and I directly used `do` notation without understanding how `>>=` exactly worked. I guess I was put off by the `Monad` of the type signature...).

So, I started to build a solution without using the case clause. My first approach was:

```haskell
p1 <*> p2 = Parser p
    where p str = let res = runParser p1 str
                  in fmap (\r -> runParser (fst r <$> p2) (snd r)) res
```

Obviously this doesn't work. The problem is that the expected type of `p` is `String -> Maybe (b, String)` and the actual type is `String -> Maybe (Maybe (b, String))`. A `Maybe` inside a `Maybe`, how annoying is that! I tried to make it work but there was no easy way of getting rid of that. The problem is that `res` is of type `Maybe ((a -> b), String)` and the function in `fmap` is `Maybe ((a -> b), String) -> Maybe (b, String)`. So indeed, the whole signature of `fmap` is `(Maybe ((a -> b), String) -> Maybe (b, String)) -> Maybe (Maybe ((a -> b), String)) -> Maybe (Maybe (b, String))`.

Here is where `>>=` comes to the recue! The signature is `Maybe ((a -> b), String) -> (((a -> b), String) -> Maybe (b, String)) -> Maybe (b, String)`. So we just have to write a function of type `((a -> b), String) -> Maybe (b, String)`, which we called `g` in the solution:

```haskell
p1 <*> p2 = Parser p
    where p s      = runParser p1 s >>= g
          g (f, r) = runParser (f <$> p2) r
```

Finally, I just leave this [post](https://pbrisbin.com/posts/maybe_is_just_awesome/) which helped me to understand why `Maybe` is a `Monad` and how it can be used.

## Exercise 3

After having completed the second exercise, we can start creating parsers in a more convenient way.

The first parser we will create is `abParser :: Parser (Char, Char)` which expects the characters 'a' and 'b' and returns them as a pair, and it fails otherwise. There is a cool function to create tuples: `(,) :: a -> b -> (a, b)` (by adding commas, you add additional elements). Then, the solution is:

```haskell
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
```

Note that the expression `(,) <$> char 'a' <*> char 'b'` is equivalent to `((,) <$> char 'a') <*> char 'b'`. We first compute `(,) <$> char 'a'` which has type `Parser (Char -> (Char, Char))`. Then we apply `char 'b'` to that function and we obtain `Parser (Char, Char)`, which is what we want.

Next we have to implement `abParser_ :: Parser ()` which acts the same way as the previous one except it returns `()` instead of the characters 'a' and 'b'. We have to transform `Parser (Char, Char)` into `Parser ()` so we can just use `fmap`. Note that we use `const :: a -> b -> a` to always return `()`:

```haskell
abParser_ :: Parser ()
abParser_ = const () <$> abParser
```

The last parser to implement is `intPair :: Parser [Integer]` which expects two integers separated by a space and returns them as a list:

```haskell
intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt
```

## Exercise 4

In this exercise we make `Parser` an instance of `Alternative` type class. We can take advantage of `Maybe` already having defined the `Alternative` type class:

```haskell
instance Alternative Parser where
  empty = Parser $ const Nothing

  p1 <|> p2 = Parser p
    where p s = runParser p1 s <|> runParser p2 s
```

Note that instead of writing `\_ -> Nothing` we can write `const Nothing`.

## Exercise 5

In the last exercise we implement a parser `intOrUppercase :: Parser ()` that expects either an integer or an uppercase character. It's easy using the `<|>` operator. By the way, note that the priority of `<|>` is lower than `<$>`, so we don't even need parentheses:

```haskell
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
```
