# Homework 11

This homework contains the following files:
* [Assignment](assignment.pdf)
* [AParser.hs](AParser.hs)
* [SExpr.hs (file to submit)](SExpr.hs)

In this assignment we extend the previous homework so make sure you understood everything we did there. Just remember that `Parser` is an instance of `Functor`, `Applicative` and `Alternative` type classes. With that we can write code that does not need to know the `Parser` implementation details.

## Exercise 1

We have to implement the parsers `zeroOrMore :: Parser a -> Parser [a]` and `oneOrMore :: Parser a -> Parser [a]`. The hint that they give us is kinda important since it tells us how to do it. Indeed, we can define `zerOrMore` and `oneOrMore` in terms of each other. The key is that the latter will always consume some input so we will eventually exhaust it.

Even if the solution looks short and simple, it took me a while to get it, so don't assume that a solution is obvious and effortless just because it's short.

```haskell
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p
```

In `oneOrMore` we run the parser *p* once and then we parse zero or more occurrences of *p*. Since *p* has type `Parser a` and `zeroOrMore p` has type `Parser [a]`, we have to combine them somehow. We can just cons the former to the latter by lifting the `:` operator which is defined for lists.

In `zeroOrMore` we try to parse one or more occurrences of *p* and if it fails we return an empty list. Since we need to return a `Parser [a]`, we can't just return an empty list as it is, we have to wrap it in a `Parser`. This is what `pure does`. Remember that the `p1 <|> p2` tries to parse using `p1` and if it fails, uses `p2`.


## Exercise 2

The parser `spaces :: Parser String` parses a consecutive list of zero or more whitespace characters. This is easy to implement using the `zeroOrMore` function we defined in the previous exercise:

```haskell
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace
```

The parser `ident :: Parser String` parses an identifer which is an alphabetic character followed by zero or more alphanumeric characters. The implementation is straightforward:

```haskell
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
```

To combine the results of the two parsers, we lift the cons operator.

## Exercise 3

The last exercise consisted on parsing *S-expressions*. We are already given the representation of those kind of expresions:

```haskell
type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
```

We must take into account that S-expressions may begin or end with any number of spaces, so we have to ignore them. In order to that we can use `(*>) :: Applicative f => f a -> f b -> f b` and `(<*) :: Applicative f => f a -> f b -> f a` (even if we ignore spaces, we first have to parse them). Moreover we also have to take into consideration that an S-expression is either an atom or an open parenthesis followed by one or more S-expressions followed by a close parenthesis. We can directly translate that construction into code:

```haskell
parseAtom :: Parser SExpr
parseAtom =  fmap A $ N <$> posInt <|> I <$> ident

parseComb :: Parser SExpr
parseComb = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
```

I loved this assignment because by writing very few lines of code (all the functions are one-liners!) we were able to create a powerful parser. In addition, it is quite readable as it mirrors the way the language is defined and I feel like there was only one way of implementing those functions, which is nice since you do not have to wonder about which solution is more adequate.
