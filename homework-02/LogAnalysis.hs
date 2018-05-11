{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


---------------------------------- Exercise 1 ----------------------------------

createInfoMessage :: [String] -> LogMessage
createInfoMessage (timestamp:content) =
  LogMessage Info (read timestamp) (unwords content)
createInfoMessage msg = error ("Invalid Info message: " ++ unwords msg)

createWarningMessage :: [String] -> LogMessage
createWarningMessage (timestamp:content) =
  LogMessage Warning (read timestamp) (unwords content)
createWarningMessage msg = error ("Invalid Warning message: " ++ unwords msg)

createErrorMessage :: [String] -> LogMessage
createErrorMessage (severity:timestamp:content) =
  LogMessage (Error (read severity)) (read timestamp) (unwords content)
createErrorMessage msg = error ("Invalid Error message: " ++ unwords msg)

createUnknownMessage :: [String] -> LogMessage
createUnknownMessage message = Unknown (unwords message)

-- Parse an individual message from the log file
parseMessage :: String -> LogMessage
parseMessage ('I':message) = createInfoMessage    $ words $ message
parseMessage ('W':message) = createWarningMessage $ words $ message
parseMessage ('E':message) = createErrorMessage   $ words $ message
parseMessage message       = createUnknownMessage $ words $ message

parseMessageTest :: Bool
parseMessageTest = and
  [
    LogMessage Info 29 "la la la" == parseMessage "I 29 la la la",
    LogMessage Warning 19 "le le le" == parseMessage "W 19 le le le",
    LogMessage (Error 2) 562 "help help" == parseMessage "E 2 562 help help",
    Unknown "Not in the right format" == parseMessage "Not in the right format"
  ]

-- Parse a whole log file
parse :: String -> [LogMessage]
parse file = parseMessage <$> lines file


---------------------------------- Exercise 2 ----------------------------------

-- Insert a new LogMessage into an existing MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg left) msg2 right
  | otherwise = Node left msg2 (insert msg right)
insert _ (Node _ (Unknown _) _) =
  error "Unknown messages are not allowed in MessageTree"

insertTest :: Bool
insertTest = and
  [
    Node Leaf info Leaf == insert info Leaf,
    Node Leaf info (Node Leaf warning Leaf) == insert warning infoTree,
    Node (Node Leaf info Leaf) warning Leaf == insert info warningTree
  ]
  where info        = LogMessage Info 30 "doesn't matter"
        infoTree    = Node Leaf info Leaf
        warning     = LogMessage Warning 50 "doesn't matter"
        warningTree = Node Leaf warning Leaf


---------------------------------- Exercise 3 ----------------------------------

-- Build a complete MessageTree from a list of LogMessage
-- Note that we must use foldr and not foldl due to insert type signature
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

buildTest :: Bool
buildTest = and
  [
    Node Leaf info Leaf == build [info],
    Node (Node Leaf info Leaf) warning Leaf == build [info, warning],
    Node Leaf info (Node Leaf warning Leaf) == build [warning, info, unknown]
  ]
  where info    = LogMessage Info 10 "doesn't matter"
        warning = LogMessage Warning 20 "doesn't matter"
        unknown = Unknown "doesn't matter"


---------------------------------- Exercise 4 ----------------------------------

-- Traverses a MessageTree in inorder:
--    1) Traverse the left subtree
--    2) Visit the root
--    3) Traverse the right subtree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

inOrderTest :: Bool
inOrderTest = and
  [
    [info] == inOrder (Node Leaf info Leaf),
    [info, warning] == inOrder (Node (Node Leaf info Leaf) warning Leaf)
  ]
  where info    = LogMessage Info 10 "doesn't matter"
        warning = LogMessage Warning 20 "doesn't matter"


---------------------------------- Exercise 5 ----------------------------------

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = severity >= 50
isRelevant _                                 = False

getContent :: LogMessage -> String
getContent (LogMessage _ _ content) = content
getContent (Unknown content)        = content

-- Return the content of relevant error messages sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = getContent <$> filter isRelevant (inOrder . build $ xs)

whatWentWrongTest :: Bool
whatWentWrongTest = and
  [
    [] == whatWentWrong [info, warning, error1],
    ["second error"] == whatWentWrong [info, warning, error2],
    ["second error", "third error"] == whatWentWrong [error1, error2, error3]
  ]
  where info    = LogMessage Info 10 "doesn't matter"
        warning = LogMessage Warning 20 "doesn't matter"
        error1  = LogMessage (Error 20) 1 "first error"
        error2  = LogMessage (Error 60) 2 "second error"
        error3  = LogMessage (Error 80) 3 "third error"
