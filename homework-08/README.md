# Homework 8

This homework contains the following files:
* [Assignment](assignment.pdf)
* [Employee.hs (provided)](Employee.hs)
* [company.txt (provided)](company.txt)
* [Party.hs (file to submit)](Party.hs)

In this homework we work with the following definitions:

```haskell
type Name = String

type Fun  = Integer

data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

data GuestList = GL [Employee] Fun
  deriving (Show, Eq)
```

For a more detailed explanation on what those types represent, read the [assignment](assignment.pdf).

## Exercise 1

The function `glCons :: Employee -> GuestList -> GuestList` adds an employee to a guest list ignoring any constraints. We just have to pattern match the `GuestList` and add the employee. Note that we cons the new employee since it is more efficient than doing `xs ++ [x]`. We also use `empFun :: Employee -> Fun` to get the amout of fun of an employee:

```haskell
glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (x:xs) (f + empFun x)
```

The second function is just about defining a `Monoid` instance for `GuestList`:

```haskell
instance Monoid GuestList where
  mempty                      = GL [] 0
  GL xs f1 `mappend` GL ys f2 = GL (xs ++ ys) (f1 + f2)
```

The third function is `moreFun :: GuestList -> GuestList -> GuestList` which returns the guest list which has the higher fun score. If we take a look at [Employee.hs](Employee.hs) file, we see that the following instance is already defined:

```haskell
instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2
```

So we can just use `max :: Ord a => a -> a -> a` to do the job:

```haskell
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
```

## Exercise 2

In this exercise we had to implement a fold for the type `Tree` which is defined as follows in `Data.Tree` module:

```haskell
data Tree a = Node {rootLabel :: a, subForest :: Forest a}
type Forest a = [Tree a]
```

The signature of the function is not given to us, so we can take a look at how it was defined in the previous lecture:

```haskell
data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)
```

First of all we realise that the data types for `Tree` are different. In the former we can't represent an empty tree, since the smallest tree that can be represented is `Node a []`, which is a leaf. So we do not need a base element in the fold function, since we will always start with the leaves. From that we can deduce that in our case the signature will be: `treeFold :: (a -> [b] -> b) -> Tree a -> b` and its implementation is:

```haskell
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a sf) = f a (treeFold f <$> sf)
```

Note that in order to fold a node, we fold each sub forest of that node and then we combine the results with the function `a -> [b] -> b`, where `a` is the root label of the current node and `[b]` are the results of folding each sub forest. So we are folding from the leaves to the root.

## Exercise 3

The function `nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)` looks intimidating at first but at the end of the day it is not that difficult to implement:

```haskell
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss withoutBosses, withBosses)
  where withBosses    = mconcat $ fst <$> gls
        withoutBosses = mconcat $ snd <$> gls
```

We merge all the first elements from `gls`, which are subtrees with bosses, into one guest list and we just let it go through as the second element of the result. Indeed, if we decide to take all the bosses from the subtree, we can't pick the boss of the current node. Meanwhile, we also merge all the second elements from `gls`, which are subtrees without bosses, and we add the boss of the current node. Finally, I realised that the following function exists:

```haskell
foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap g = mconcat . fmap g
```

So there is a more elegant solution:

```haskell
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss withoutBosses, withBosses)
  where withBosses    = foldMap fst gls
        withoutBosses = foldMap snd gls
```

## Exercise 4

We now have to define `maxFun :: Tree Employee -> GuestList` which takes a tree and returns the guest list which maximizes the fun of the employees. It is kinda obvious that we have to make use of the `treeFold` function implemented before. It has the following signature if we use `maxFun` as fold function: `treeFold :: (Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)) -> Tree Employee -> (GuestList, GuestList)`. So it is pretty obvious that we can just use `nextLevel` as it is.

The problem is that `treeFold nextLevel` returns a pair of guest lists, and `maxFun` needs to return a single one. Which one of the two should we take? Well, the one that maximizes the fun! We already have implemented `moreFun :: GuestList -> GuestList -> GuestList` and we want to apply it to `(GuestList, GuestList)`. We can use `uncurry :: (a -> b -> c) -> (a, b) -> c` which is already defined to help us to write a one-liner solution:

```haskell
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
```

## Exercise 5

In this last exercise we had to read a company's hierarchy from [company.txt](company.txt) file and print a formatted list of the employees that should be invited to the party in order to maximize the amount of fun. Moreover, the output list must contain the names of the employees sorted by first name and one employee per line.

This is the first time that we will use `IO` but it is pretty easy: we only have to read a file using `readFile :: FilePath -> IO String` and then print the result using `putStr :: String -> IO ()`. We will build a single `String` with all the result, so we will call `putStr` once.

We have defined two functions to format `GuestList` and `[Employee]` as we want:

```haskell
formatEmp :: [Employee] -> String
formatEmp = unlines . sort . fmap empName

formatGL :: GuestList -> String
formatGL (GL xs fun) = "Total fun: " ++ show fun ++ "\n" ++ formatEmp xs
```

Then, the main function is pretty short:

```haskell
main :: IO()
main = do
  contents <- readFile "company.txt"
  putStr . formatGL . maxFun . read $ contents
```

Note that I have used a more readable way of dealing with `IO` than the one that was explained in the notes, but I had already read [this](http://learnyouahaskell.com/input-and-output) so for me it made more sense that way.

Finally and for the record, at first I sorted `[Employee]` in those two alternative ways (then I realised it was simpler to project the name first and then sort, since we don't need to print the amount of fun of each employee in the result):

```haskell
formatEmp :: [Employee] -> String
formatEmp = unlines . fmap empName . sortOn empName

formatEmp :: [Employee] -> String
formatEmp = unlines . fmap empName . sortBy (compare `on` empName)
```
