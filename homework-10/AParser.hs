{-# OPTIONS_GHC -Wall #-}

module AParser where


import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


---------------------------------- Exercise 1 ----------------------------------

-- Applies a function to the first component of a tuple
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p


---------------------------------- Exercise 2 ----------------------------------

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  p1 <*> p2 = Parser p
    where p s      = runParser p1 s >>= g
          g (f, r) = runParser (f <$> p2) r


---------------------------------- Exercise 3 ----------------------------------

-- A parser that expects the characters 'a' and 'b'
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- Same as abParser excepts it returns () instead of characters 'a' and 'b'
abParser_ :: Parser ()
abParser_ = const () <$> abParser

-- A parser that expects two integers separated by a space
intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt


abParserTest :: Bool
abParserTest = and
  [
     Just (('a','b'), "cd") == runParser abParser "abcd",
     Nothing                == runParser abParser "acbb"
  ]

abParser_Test :: Bool
abParser_Test = and
  [
     Just ((),"cd") == runParser abParser_ "abcd",
     Nothing        == runParser abParser_ "acbb"
  ]

intPairTest :: Bool
intPairTest = and
  [
     Just ([123, 456], "") == runParser intPair "123 456",
     Just ([1, 2], " abc") == runParser intPair "1 2 abc",
     Nothing               == runParser intPair "123,456",
     Nothing               == runParser intPair "abc cde"
  ]


---------------------------------- Exercise 4 ----------------------------------

instance Alternative Parser where
  empty = Parser $ const Nothing

  p1 <|> p2 = Parser p
    where p s = runParser p1 s <|> runParser p2 s


---------------------------------- Exercise 5 ----------------------------------

-- A parser that expects either an integer or an uppercase character
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper


intOrUppercaseTest :: Bool
intOrUppercaseTest = and
  [
    Just ((),"ab") == runParser intOrUppercase "123ab",
    Just ((),"b1") == runParser intOrUppercase "Ab1",
    Just ((),"AB") == runParser intOrUppercase "12AB",
    Nothing        == runParser intOrUppercase "a123b"
  ]
