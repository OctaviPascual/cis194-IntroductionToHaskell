{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee


---------------------------------- Exercise 1 ----------------------------------

-- Adds an employee to the guest list ignoring constraints
glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (x:xs) (f + empFun x)

instance Monoid GuestList where
  mempty                      = GL [] 0
  GL xs f1 `mappend` GL ys f2 = GL (xs ++ ys) (f1 + f2)

-- Returns the guest list which has the higher fun score
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


glConsTest :: Bool
glConsTest = and
  [
    GL [joe] 3         == glCons joe (GL [] 0),
    GL [sam,joe] 7     == glCons sam (GL [joe] 3),
    GL [sue,sam,joe] 8 == glCons sue (GL [sam,joe] 7)
  ]
  where joe = Emp "Joe" 3
        sam = Emp "Sam" 4
        sue = Emp "Sue" 1

instanceTest :: Bool
instanceTest = and
  [
    GL [] 0        == mempty,
    GL [joe,sam] 7 == glCons joe mempty <> glCons sam mempty
  ]
  where joe = Emp "Joe" 3
        sam = Emp "Sam" 4

moreFunTest :: Bool
moreFunTest = and
  [
    mempty     == moreFun mempty mempty,
    GL [joe] 7 == moreFun (glCons joe mempty) (glCons joe mempty),
    GL [joe] 7 == moreFun (glCons joe mempty) (glCons sam mempty)
  ]
    where joe = Emp "Joe" 7
          sam = Emp "Sam" 4


---------------------------------- Exercise 2 ----------------------------------

-- Fold a Tree from the leaves to the root
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a sf) = f a (treeFold f <$> sf)


treeFoldTest :: Bool
treeFoldTest = and
  [
    8 == (treeFold (\_ xs -> 1 + sum xs) testCompany :: Integer),
    17 == treeFold (\x xs -> max (empFun x) (myMaximum xs)) testCompany,
    46 == treeFold (\x xs -> empFun x + sum xs) testCompany
  ]
  where myMaximum [] = 0
        myMaximum xs = maximum xs


---------------------------------- Exercise 3 ----------------------------------

-- Returns a pair of guest lists:
-- * the first is the best list including the current boss
-- * the second is the best list wihout including the current boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss withoutBosses, withBosses)
  where withBosses    = foldMap fst gls
        withoutBosses = foldMap snd gls


nextLevelTest :: Bool
nextLevelTest = (GL [joe] 5, gl) == nextLevel joe [(gl, mempty)]
  where joe = Emp "Joe" 5
        sam = Emp "Sam" 2
        sue = Emp "Sue" 2
        gl  = GL [sam,sue] 4

---------------------------------- Exercise 4 ----------------------------------

-- Returns a guest list that maximizes the fun for the given tree
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


maxFunTest :: Bool
maxFunTest = and
  [
    GL [joe] 5     == maxFun (Node joe []),
    GL [sam,sue] 6 == maxFun (Node joe [Node sam [], Node sue []])
  ]
  where joe = Emp "Joe" 5
        sam = Emp "Sam" 3
        sue = Emp "Joe" 3

---------------------------------- Exercise 5 ----------------------------------

formatEmp :: [Employee] -> String
formatEmp = unlines . sort . fmap empName

formatGL :: GuestList -> String
formatGL (GL xs fun) = "Total fun: " ++ show fun ++ "\n" ++ formatEmp xs

main :: IO()
main = do
  contents <- readFile "company.txt"
  putStr . formatGL . maxFun . read $ contents
