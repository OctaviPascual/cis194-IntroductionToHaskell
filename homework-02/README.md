# Homework 2

This homework contains the following files:

- [Assignment](assignment.pdf)
- [Log.hs (provided)](Log.hs)
- [error.log (provided)](error.log)
- [sample.log (provided)](sample.log)
- [LogAnalysis.hs (file to submit)](LogAnalysis.hs)

Next I try to explain how I solved each exercise.

## Exercise 1

The function `parseMessage :: String -> LogMessage` parses an individual line from the log file. First of all we use pattern matching to figure out what type of log message we are dealing with. Then we simply use the `words :: String -> [String]` function to transform the remaining line into a `[String]`. Finally we use helper functions to pattern match the fields that we need for each type of message. Note that we use `unwords :: [String] -> String` function, which is the inverse of `words`, to get back the content of the message as a `String`.

```haskell
parseMessage :: String -> LogMessage
parseMessage ('I':message) = createInfoMessage    $ words $ message
parseMessage ('W':message) = createWarningMessage $ words $ message
parseMessage ('E':message) = createErrorMessage   $ words $ message
parseMessage message       = createUnknownMessage $ words $ message
```

The function `parse :: String -> [LogMessage]` parses the whole log file. To do that we just have to apply the previous `parseMessage` function to each line. The more elegant solution uses the operator `<$>` which is the infix version of `fmap`.

```haskell
parse :: String -> [LogMessage]
parse file = parseMessage <$> lines file
```

## Exercise 2

The function `insert :: LogMessage -> MessageTree -> MessageTree` inserts a log message into a binary search tree sorted by timestamp. If the message is unknown, it is not stored in the tree.

```haskell
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg left) msg2 right
  | otherwise = Node left msg2 (insert msg right)
insert _ (Node _ (Unknown _) _) =
  error "Unknown messages are not allowed in MessageTree"
```

At first I thought that with the guard `insert (Unknown _) tree = tree` I had already dealt with unknown messages. Nonetheless, a `-Wincomplete-patterns` warning message appeared. Indeed, the type signature of the `insert` function does not ensure that the tree is free of unknown messages. For example, the following expression is valid `insert (LogMessage Info 30 "doesn't matter") (Node Leaf (Unknown "doesn't matter") Leaf)` and would make the function crash. Since trees with unknown messages can be represented, we must deal with that case. All in all, Haskell did the work for us and found an edge case that we missed!

## Exercise 3

The function `build :: [LogMessage] -> MessageTree` just builds a tree from a list of messages. My first implementation used recursion.

```haskell
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)
```

However, we can get a more elegant solution using `foldr`. Usually, we can use either `foldr` or `foldl` to avoid recursion on lists. [Here](https://gist.github.com/CMCDragonkai/9f5f75118dda10131764) you can find a more detailed explanation on how they work. In this case we must use `foldr` due to the type signature of the `insert` function. Had it been defined like `insert :: MessageTree -> LogMessage -> MessageTree` and we would have used `foldl`.

```haskell
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
```

Note that `fold` is evaluated from the right to the left, so the first message that will be inserted to the tree is the last one in the list, while the last message to be inserted will be the first in the list.

### Exercise 4

The function `inOrder :: MessageTree -> [LogMessage]` just traverses the tree in inorder. That's pretty simple to do in Haskell!

```haskell
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)
```

### Exercise 5

The function `whatWentWrong :: [LogMessage] -> [String]` returns a list of the content of the error messages that are relevant, sorted by timestamp. To do that, we build and traverse in inorder the sorted message tree, then we filter the relevant messages. Finally we just get the content of those messages.

```haskell
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = getContent <$> filter isRelevant (inOrder . build $ xs)
```
