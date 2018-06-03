{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}


module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

---------------------------------- Exercise 1 ----------------------------------

-- Evaluates an expression ExprT
eval :: ExprT -> Integer
eval (ExprT.Lit x)   = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y


evalTest :: Bool
evalTest = and
  [
    10 == eval (ExprT.Lit 10),
    10 == eval (ExprT.Add (ExprT.Lit (-10)) (ExprT.Lit 20)),
    20 == eval (ExprT.Mul (ExprT.Lit 5) (ExprT.Lit 4)),
    20 == eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4))
  ]


---------------------------------- Exercise 2 ----------------------------------

-- Evaluates an expression given as a String
-- Returns Nothing if the expression is not well-formed
-- Otherwise, it returns Just n if the expression evaluates to n
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


evalStrTest :: Bool
evalStrTest = and
  [
    Just 10 == evalStr "10",
    Just 20 == evalStr "15 + 5",
    Just 20 == evalStr "5*4",
    Nothing == evalStr "2+*3"
  ]


---------------------------------- Exercise 3 ----------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Constrain the type of its argument to ExprT
reify :: ExprT -> ExprT
reify = id


reifyTest :: Bool
reifyTest = and
  [
    ExprT.Lit 2 == (reify $ lit 2),
    ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3) == (reify $ add (lit 2) (lit 3)),
    ExprT.Mul (ExprT.Lit 5) (ExprT.Lit 6) == (reify $ mul (lit 5) (lit 6))
  ]


---------------------------------- Exercise 4 ----------------------------------

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (<= 0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x                 = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x+y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x*y) `mod` 7


instancesTest :: Bool
instancesTest = and
  [
     Just (-7) == (parseExp lit add mul "(3 * -4) + 5" :: Maybe Integer),
     Just False == (parseExp lit add mul "(3 * -4) + 5" :: Maybe Bool),
     Just (MinMax 5) == (parseExp lit add mul "(3 * -4) + 5" :: Maybe MinMax),
     Just (Mod7 0) == (parseExp lit add mul "(3 * -4) + 5" :: Maybe Mod7)
  ]


---------------------------------- Exercise 5 ----------------------------------

instance Expr Program where
  lit a   = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
run = execute . compile
  where execute Nothing  = Left "The program does not compile."
        execute (Just p) = stackVM p


compileTest :: Bool
compileTest = and
  [
    Nothing == compile "5+*2",
    Just [PushI 5] == compile "5",
    Just [PushI 5,PushI 6,StackVM.Add] == compile "5+6",
    Just [PushI 5,PushI 6,StackVM.Mul] == compile "5*6",
    Just [PushI 4,PushI 2,PushI 3,StackVM.Mul,StackVM.Add] == compile "4+2*3",
    Just [PushI 4,PushI 2,StackVM.Add,PushI 3,StackVM.Mul] == compile "(4+2)*3"
  ]

runTest :: Bool
runTest = and
  [
    Left "The program does not compile." == run "5+*2",
    Right (IVal 5) == run "5",
    Right (IVal 11) == run "5+6",
    Right (IVal 30) == run "5*6",
    Right (IVal 10) == run "4+2*3",
    Right (IVal 18) == run "(4+2)*3"
  ]


---------------------------------- Exercise 6 ----------------------------------

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Calc.Var

type MapSI = M.Map String Integer

instance HasVars (MapSI -> Maybe Integer) where
  var = M.lookup

instance Expr (MapSI -> Maybe Integer) where
  lit a _   = Just a
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> (MapSI -> Maybe Integer) -> Maybe Integer
withVars vars expr = expr $ M.fromList vars


instanceTest :: Bool
instanceTest = and
  [
    Calc.Lit 5   == (lit 5 :: VarExprT),
    Calc.Var "x" == (var "x" :: VarExprT),
    Calc.Mul (Calc.Var "x") (Calc.Lit 5) == (mul (var "x") (lit 5) :: VarExprT),
    Calc.Add (Calc.Lit 3) (Calc.Var "x") == (add (lit 3) (var "x") :: VarExprT)
  ]

withVarsTest :: Bool
withVarsTest = and
  [
    Nothing == (withVars [] $ var "x"),
    Just 6 == (withVars [("x",6)] $ add (lit 3) (lit 3)),
    Just 9 == (withVars [("x",6)] $ add (lit 3) (var "x")),
    Just 9 == (withVars [("y",3)] $ mul (lit 3) (var "y")),
    Just 15 == (withVars [("x", 5), ("y", 3)] $ mul (var "x") (var "y"))
  ]
