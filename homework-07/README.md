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

The first function we will implement is `(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a` which appends two join-lists. The monoidal annotation of the new join-list is derived from the annotations of the two original join-lists.

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

Here we use the `(<>) :: Monoid m => m -> m -> m` function, which is just the infix form of `mappend`. This function takes two values from the same type and produces a new value.


## Exercise 2

### Initial solution

The `indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a` function finds the `JoinList` element at the specified index. We must implement this taking advantage of the annotations, which are instace of `Sized` typeclass. The structure of the function is similar to a search in a binary tree, except that here, apart from deciding whether the index we are looking for is in the left or the right join-list, we must update the index value if we visit the right one:

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

Note that we assume that the join-list is well constructed, that is, the sizes in the annotations are consistent. Also, to transform an annotation into an `Int` we compose the functions `getSize :: Size -> Int` and `size :: Sized a => a -> Size`.

The second function is `dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a` which drops the first *n* elements of the `JoinList`. It is similar to the `drop :: Int -> [a] -> [a]` function for lists. The difficulty here is that we cannot just remove the elements from the `JoinList`, we must also keep the annotations up to date. So, when we remove something, we must make sure that annotations keep being consistent. Before showing the solution, let's refresh how `drop` for lists could be implemented:

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

Now you might wonder how do we make sure that the annotations are kept up to date. Well, the magic happens in `dropJ n jl1 +++ jl2` and `dropJ n jl1 +++ dropJ (n - left) jl2` expressions. We use the append operator that we have defined before in order to update the annotations. By the way, we compare `n` with `left`, which is the annotation on the left join-list, in order to know if we will have to modify or not the right join-list. If the answer is yes (which means that all the elements from the left list have been droppped), we cannot just use `n` as it is, we must subtract all the elements from the left. That's why we do `n - left`.

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

### Reviewed solution

After finishing the whole assignment, I checked [this](https://github.com/baugarten/CIS-194) repository and realised there was a more elegant and efficient solution.

The first thing we notice is that I have used the same `where` clause in `indexJ`, `dropJ` and `takeJ`. We can define a function that does that:

```haskell
getSizeTag :: (Monoid b, Sized b) => JoinList b a -> Int
getSizeTag = getSize . size . tag
```

Next, in `dropJ` and `takeJ` we find the following expressions: `otherwise = dropJ n jl1 +++ dropJ (n - left) jl2` and `otherwise = takeJ n jl1 +++ takeJ (n - left) jl2`. When we end up in the otherwise guard, it means that `n >= left` in both cases. That means that we want to drop/take more elements than the size of the left join-list. But do we really need to make the recursive call? Not really, because we already know that we will end up dropping all the elements from the left join-list, so we could already write `otherwise = dropJ (n - left) jl2`. The same idea can be applied for take: we already know we will take all the elements from the left join-list, so we can write `otherwise = jl1 +++ takeJ (n - left) jl2`.

Taking all of the previous considerations into account, let's see the reviewed versions of the functions. In `indexJ` we check the edge cases before pattern matching `Single` and `Append`. Note that when we pattern match `Single`, we are certain that the value of the index will be 0.

```haskell
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                   = Nothing
indexJ i _  | i < 0              = Nothing
indexJ i jl | i >= getSizeTag jl = Nothing
indexJ _ (Single _ a)            = Just a
indexJ i (Append _ jl1 jl2)
  | i < left                     = indexJ i jl1
  | otherwise                    = indexJ (i - left) jl2
  where left = getSizeTag jl1
```

In `dropJ`, we already discard the whole left join-list in `otherwise` clause. The structure is similar to `indexJ`:

```haskell
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                   = Empty
dropJ n jl | n <= 0             = jl
dropJ n jl | n >= getSizeTag jl = Empty
dropJ _ (Single _ _)            = Empty
dropJ n (Append _ jl1 jl2)
  | n < left                    = dropJ n jl1 +++ jl2
  | otherwise                   = dropJ (n - left) jl2
  where left = getSizeTag jl1
```

Finally, `takeJ`:

```haskell
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                   = Empty
takeJ n _  | n <= 0             = Empty
takeJ n jl | n >= getSizeTag jl = jl
takeJ _ jl@(Single _ _)         = jl
takeJ n (Append _ jl1 jl2)
  | n < left                    = takeJ n jl1
  | otherwise                   = jl1 +++ takeJ (n - left) jl2
  where left = getSizeTag jl1
```

By the way, having tests has made it really easy to refactor those methods, I'm glad I wrote them :)

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

The function `score :: Char -> Score` returns the score of a character. Since characters have different scores, we define the score of each one in `scores :: [(Char,Int)]` which is just a constant. This format is appropriate as we can use the already defined function `lookup :: Eq a => a -> [(a, b)] -> Maybe b` to get the score associated with each character.

```haskell
score :: Char -> Score
score c = maybe 0 Score $ lookup (toUpper c) scores
```

Remember that the type signature of `maybe` is `b -> (a -> b) -> Maybe a -> b` so the type constructor `Score` is used as the `(a -> b)` function (the signature of `Score` is `Int -> Score`).

Next we must define a function `scoreString :: String -> Score` which returns the score of a string. It is tempting to just do the following, since it works:

```haskell
scoreString :: String -> Score
scoreString = sum . fmap score
```

But that works because `Score` derives from `Num` and we know that to combine two scores we must add them. But at the beginning of this exercise we have made `Score` an instance of `Monoid` and we have already defined there how to combine two scores. So it is redundant to redefine this behaviour here. Instead, we can use the `mconcat :: Monoid a => [a] -> a` which will reduce all the scores into a single one, using the `mappend` operation that is defined inside `Score`. It is defined like this: `mconcat = foldr mappend mempty`. So the final solution is:

```haskell
scoreString :: String -> Score
scoreString = mconcat . fmap score
```

All in all, it looks almost the same but the meaning is totally different! Here we are just delegating the responsability to `Score`, taking advantage of the fact that it is a `Monoid`. For example, imagine that instead of adding the scores we want to multiply them, then we would just change `mappend = (+)` for `mappend = (*)` in the instance definition and we wouldn't have to change `scoreString`.

To end the exercise, we must define `scoreLine :: String -> JoinList Score String` which returns a `JoinList` with the score of the string as annotation:

```haskell
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
```

## Exercise 4

The last exercise was to make the type `JoinList (Score, Size) String` an instance of `Buffer`. It was more challenging than it seems, but in the end the solution is pretty short since all the functions that were defined in previous exercises could be reused here:

```haskell
instance Monoid m => Monoid (JoinList m a) where
  mempty  = Empty
  mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
  toString          = unlines . jlToList
  fromString        = mconcat . fmap createJoinList . lines
    where createJoinList s = Single (scoreString s, Size 1) s
  line              = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines          = getSize  . snd . tag
  value             = getScore . fst . tag
```

First off, we have also made `JoinList` an instance of `Monoid` in order to use `mconcat` in `fromString`. If you think about this, this makes sense since we have defined the concept of an empty element and the append operator for join-lists. Making use of `mconcat`, the solution of `fromString` is pretty consice. However, I think it is not the most efficient way to build the structure since we end up with an unbalanced tree. We set `Size 1` since each join-list we are creating is a line, and the second element of the annotation tracks the number of lines of the text. The other functions are pretty self-explanatory.

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
