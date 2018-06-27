{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char


---------------------------------- Exercise 3 ----------------------------------

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scores :: [(Char,Int)]
scores =
  [
    ('A',1),('E',1),('I',1),('L',1),('N',1),('O',1),('R',1),('S',1),('T',1),('U',1),
    ('D',2),('G',2),
    ('B',3),('C',3),('M',3),('P',3),
    ('F',4),('H',4),('V',4),('W',4),('Y',4),
    ('K',5),
    ('J',8),('X',8),
    ('Q',10),('Z',10)
  ]

-- Return the score of a character
score :: Char -> Score
score c = maybe 0 Score $ lookup (toUpper c) scores

-- Return the score of a string
scoreString :: String -> Score
scoreString = mconcat . fmap score

getScore :: Score -> Int
getScore (Score n) = n


scoreTest :: Bool
scoreTest = and
  [
    Score 0 == score ' ',
    Score 0 == score '!',
    Score 1 == score 'a',
    Score 2 == score 'G',
    Score 8 == score 'x'
  ]

scoreStringTest :: Bool
scoreStringTest = and
  [
    Score 0  == scoreString "",
    Score 9  == scoreString "yay ",
    Score 14 == scoreString "haskell!"
  ]
