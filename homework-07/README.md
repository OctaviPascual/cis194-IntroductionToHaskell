# Homework 7

This homework contains the following files:
* [Assignment](assignment.pdf)
* [Editor.hs (provided)](Editor.hs)
* [Buffer.hs (provided)](Buffer.hs)
* [Sized.hs (provided)](Sized.hs)
* [StringBuffer.hs (provided)](StringBuffer.hs)
* [StringBufEditor.hs (provided)](StringBufEditor.hs)
* [carol.txt (provided)](carol.txt)
* [JoinList.hs (file to submit)](JoinList.hs)
* [Scrabble.hs (file to submit)](Scrabble.hs)

The set up of this homework is extensive, so I highly recommend to take a look at it if you need more context on how to solve each exercise.

I will only show how `JoinList` is defined:

```haskell
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)
```

## Exercise 1

The first function we will implement is `(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a` which appends two `JoinLists`. The monoidal annotation of the new `JoinList` is derived from the annotations of the two original `JoinLists`.

Note that we use the `Monoid` type class (if you are starting to worry, monoid is not the same as monad, so don't panic yet!). To understand in detail what a monoid is, visit [this](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids).

Before implementing the append function, we are advised to implement a helper function `tag :: Monoid m => JoinList m a -> m` which justs gets the annotation of a `JoinList`. This is pretty easy:

```haskell
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m
```

We can use the `mempty :: Monoid a => a` function since `m` is a `Monoid`. `mempty` is just a polymorphic constant that represents the identity value for a particular monoid.

Now we are ready to implement the append function:

```haskell
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2
```

Here we use `(<>) :: Monoid m => m -> m -> m` function, which is just the infix form of `mappend`. This function takes two values from the same type and produces a new value.


## Exercise 2

The `indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a` function finds the `JoinList` element at the specified index. We must implement this taking advantage of the annotations, which are instace of `Sized` typeclass. The structure of the function is similar to a search in a binary tree, except that here, apart from deciding whether the index we are looking for is in the left or the right `JoinList`, we must update the index value if we visit the right one:

```haskell
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing

indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing

indexJ i (Append m jl1 jl2)
  | i < 0 || i >= root = Nothing
  | i < left           = indexJ i jl1
  | otherwise          = indexJ (i - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1
```

Note that we assume that the `JoinList` is well constructed, that is, the sizes in the annotations are consistent. Also, to transform an annotation into an `Int` we compose the functions `getSize :: Size -> Int` and `size :: Sized a => a -> Size`.

The second functions is `dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a` which drops the first *n* elements of the `JoinList`. It is similar to the `drop :: Int -> [a] -> [a]` function for lists. The difficulty here is that we cannot just remove the elements from the `JoinList`, we must also keep the annotations up to date. So, when we remove something, we must make sure that annotations keep being consistent. Before showing the solution, let's refresh how `drop` for lists could be implemented:

```haskell
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (_:xs)      = drop (n-1) xs
```

We can use the same idea for our `dropJ` implementation:

```haskell
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl

dropJ _ Empty = Empty

dropJ _ (Single _ _) = Empty

dropJ n (Append m jl1 jl2)
  | n >= root = Empty
  | n < left  = dropJ n jl1 +++ jl2
  | otherwise = dropJ n jl1 +++ dropJ (n - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1
```

Now you might wonder how do we make sure that the annotations are kept up to date. Well, the magic happens in `dropJ n jl1 +++ jl2` and `dropJ n jl1 +++ dropJ (n - left) jl2`. We use the append operator that we have defined before in order to update the annotations. By the way, we compare `n` with `left`, which is the annotation on the left list, in order to know if we will have to modify or not the right list. If the answer is yes (which means that all the elements from the left list have been droppped), we cannot just use `n` as it is, we must subtract all the elements from the left. That's why we do `n - left`.

Finally, the last function is `takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a` which returns the first *n* elements of the `JoinList`. In this case, it is similar to the `take :: Int -> [a] -> [a]` function:

```haskell
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs
```

The implementation of `takeJ` has the same structure and the same ideas as `dropJ`:

```haskell
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty

takeJ _ Empty = Empty

takeJ _ jl@(Single _ _) = jl

takeJ n jl@(Append m jl1 jl2)
  | n >= root = jl
  | n < left  = takeJ n jl1
  | otherwise = takeJ n jl1 +++ takeJ (n - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1
```

## Exercise 3

In this exercise we define a new type to represent the Scrabble score. The type is named `Score` and here is how it is implemented:

```haskell
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)
```

Next we have to make `Score` an instance of Monoid:

```haskell
instance Monoid Score where
  mempty  = Score 0
  mappend = (+)
```

The function `score :: Char -> Score` returns the score of a character. Since characters have different scores, we define the score of each one in `scores :: [(Char,Int)]` which is just a constant. This format is appropriate as we can use the already defined function `lookup :: Eq a => a -> [(a, b)] -> Maybe b` to get the score of each character. 

```haskell
score :: Char -> Score
score c = maybe 0 Score $ lookup c' scores
  where c' = toUpper c
```

Remember that the type signature of `maybe` is `b -> (a -> b) -> Maybe a -> b` so the type constructor `Score` is just: `Int -> Score`.

Next we must define a function `scoreString :: String -> Score` which returns the score of a string. It is tempting to just do the following since it works:

```haskell
scoreString :: String -> Score
scoreString = sum . fmap score
```

But that works because `Score` derives from `Num` and we know that to combine two scores we must add them. But at the begining of this exercise we have made `Score` an instance of `Monoid` and we have already defined there how to combine two scores. So it is redundant to redefine this behaviour here. Instead, we can use the `mconcat :: Monoid a => [a] -> a` which will reduce all the scores into a single one, using the operation that is defined inside `Score`. It is defined like this: `mconcat = foldr mappend mempty`. So the final solution is:

```haskell
scoreString :: String -> Score
scoreString = mconcat . fmap score
```

All in all, it looks almost the same but the meaning is totally different! Here we are just delegating the responsability to `Score`, taking advantage of the fact that it is a `Monoid`. For example, imagine that instead of adding scores we want to multiply them, then we would just change `mappend = (+)` for `mappend = (*)` in the instance definition and we wouldn't have to change `scoreString`.

To end the exercise, we must define `scoreLine :: String -> JoinList Score String` which returns a `JoinList` with the score of the string as annotation:

```haskell
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
```

## Exercise 4

The last exercise was to make the type `JoinList (Score, Size) String` an instance of `Buffer`. It was more challenging than it seems, but in the end the solution is pretty short since all the functions that were defined in previous exercises could be reused here:

```haskell
instance Buffer (JoinList (Score, Size) String) where
  toString          = unlines . jlToList
  fromString        = foldr1 (+++) . fmap createJoinList . lines
    where createJoinList s = Single (scoreString s, Size 1) s
  line              = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines          = getSize  . snd . tag
  value             = getScore . fst . tag
```

In `fromString` function, we can use either `foldr1` or `foldl1` since the `+++` operation is associative. We set `Size 1` since each JoinList we are creating is a line, and the second element of the annotations tracks the number of lines of the text. The other functions are pretty self-explanatory.

To end the homework we must implement a `main` function. We can just take a look at how it is implemented in the provided [StringBufEditor.hs](StringBufEditor.hs) file and adapt it. In order to let the compiler know that we will work with the type `JoinList (Score, Size) String`, we create a helper function named `reify`.

```haskell
reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id

main :: IO()
main = runEditor editor . reify . fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
```
