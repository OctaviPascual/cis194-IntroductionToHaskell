{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Sized
import Scrabble
import Editor


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


---------------------------------- Exercise 1 ----------------------------------

-- Append two JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Get the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


appendTest :: Bool
appendTest = and
  [
    Append mempty empty empty       == empty    +++ empty,
    Append "a" singleA empty        == singleA  +++ empty,
    Append "ab" appendAB empty      == appendAB +++ empty,
    Append "aba" appendAB singleA   == appendAB +++ singleA,
    Append "ab" singleA singleB     == singleA  +++ singleB,
    Append "abba" appendAB appendBA == appendAB +++ appendBA
  ]
  where empty    = (Empty :: JoinList String Char)
        singleA  = Single "a" 'a'
        singleB  = Single "b" 'b'
        appendAB = Append "ab" singleA singleB
        appendBA = Append "ba" singleB singleA

tagTest :: Bool
tagTest = and
  [
    ""   == tag Empty,
    "a"  == tag (Single "a" 'a'),
    "ab" == tag (Append "ab" (Single "a" 'a') (Single "b" 'b'))
  ]


---------------------------------- Exercise 2 ----------------------------------


-- Get the Int value of a Sized tag
getSizeTag :: (Monoid b, Sized b) => JoinList b a -> Int
getSizeTag = getSize . size . tag

-- Finds the JoinList element at the specified index
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                   = Nothing
indexJ i _  | i < 0              = Nothing
indexJ i jl | i >= getSizeTag jl = Nothing
indexJ _ (Single _ a)            = Just a
indexJ i (Append _ jl1 jl2)
  | i < left                     = indexJ i jl1
  | otherwise                    = indexJ (i - left) jl2
  where left = getSizeTag jl1

-- Drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                   = Empty
dropJ n jl | n <= 0             = jl
dropJ n jl | n >= getSizeTag jl = Empty
dropJ _ (Single _ _)            = Empty
dropJ n (Append _ jl1 jl2)
  | n < left                    = dropJ n jl1 +++ jl2
  | otherwise                   = dropJ (n - left) jl2
  where left = getSizeTag jl1

-- Returns the first n elements from a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                   = Empty
takeJ n _  | n <= 0             = Empty
takeJ n jl | n >= getSizeTag jl = jl
takeJ _ jl@(Single _ _)         = jl
takeJ n (Append _ jl1 jl2)
  | n < left                    = takeJ n jl1
  | otherwise                   = jl1 +++ takeJ (n - left) jl2
  where left = getSizeTag jl1

-- Safe list indexing function (provided)
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

-- Convert a JoinList into a list ignoring monoidal annotations (provided)
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


indexJTest :: Bool
indexJTest = and
  [
    jlToList jl !!? i == indexJ i jl |
      i  <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC

dropJTest :: Bool
dropJTest = and
  [
    drop i (jlToList jl) == jlToList (dropJ i jl) |
      i  <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC

takeJTest :: Bool
takeJTest = and
  [
    take i (jlToList jl) == jlToList (takeJ i jl) |
      i  <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC


---------------------------------- Exercise 3 ----------------------------------

-- Return a JoinList with the score of the string as annotation
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


scoreLineTest :: Bool
scoreLineTest = and
  [
    Single (Score 0) ""     == scoreLine "",
    Single (Score 9) "yay " == scoreLine "yay "
  ]


---------------------------------- Exercise 4 ----------------------------------

instance Monoid m => Monoid (JoinList m a) where
  mempty  = Empty
  mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
  toString          = unlines . jlToList
  fromString        = mconcat . fmap createJoinList . lines
    where createJoinList s = Single (scoreString s, Size 1) s
  line              = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines          = getSizeTag
  value             = getScore . fst . tag

reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id

main :: IO()
main = runEditor editor . reify . fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
