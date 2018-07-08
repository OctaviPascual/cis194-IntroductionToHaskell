{-# OPTIONS_GHC -Wall #-}

module SExpr where


import AParser
import Control.Applicative
import Data.Char


---------------------------------- Exercise 1 ----------------------------------

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Runs consecutively the input parser as many times as possible and returns
-- a list of the results (always succeeds)
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- Runs consecutively the input parser as many times as possible and returns
-- a list of the results (fails if it doesn't succeed at least once)
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


zeroOrMoreTest :: Bool
zeroOrMoreTest = and
  [
     Just ("ABCDE","") == runParser (zeroOrMore (satisfy isUpper)) "ABCDE",
     Just ("ABC","dE") == runParser (zeroOrMore (satisfy isUpper)) "ABCdE",
     Just ("","abcde") == runParser (zeroOrMore (satisfy isUpper)) "abcde"
  ]

oneOrMoreTest :: Bool
oneOrMoreTest = and
  [
     Just ("ABCDE","") == runParser (oneOrMore (satisfy isUpper)) "ABCDE",
     Just ("ABC","dE") == runParser (oneOrMore (satisfy isUpper)) "ABCdE",
     Just ("A","bcde") == runParser (oneOrMore (satisfy isUpper)) "Abcde",
     Nothing           == runParser (oneOrMore (satisfy isUpper)) "abcde"
  ]


---------------------------------- Exercise 2 ----------------------------------

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- Parser which parses a consecutive list of zero or more whitespace characters
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

-- Parser which parses an identifer (alphabetic character followed by zero or
-- more alphanumeric characters)
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


spacesTest :: Bool
spacesTest = and
  [
     Just ("","ABCDE")   == runParser spaces "ABCDE",
     Just ("  ","ABCDE") == runParser spaces "  ABCDE",
     Just (" ","A BCDE") == runParser spaces " A BCDE"
  ]

identTest :: Bool
identTest = and
  [
     Just ("foobar"," baz") == runParser ident "foobar baz",
     Just ("foo33fA","")    == runParser ident "foo33fA",
     Nothing                == runParser ident "2bad",
     Nothing                == runParser ident " ",
     Nothing                == runParser ident ""
  ]

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


---------------------------------- Exercise 3 ----------------------------------

-- Parser which parses an atom (either an integer or an identifier)
parseAtom :: Parser SExpr
parseAtom =  fmap A $ N <$> posInt <|> I <$> ident

-- Parser which parses an open parenthesis followed by one or more
-- S-expressions followed by a close parenthesis
parseComb :: Parser SExpr
parseComb = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')'

-- Parser which parses a whole S-expression
parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
