{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad.Loops (iterateUntilM)
import Data.List (sortBy)


---------------------------------- Exercise 1 ----------------------------------

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)


---------------------------------- Exercise 2 ----------------------------------

-- Returns the maximum number of attackers for a given army
maxAttackers :: Army -> Int
maxAttackers n = min 3 (n-1)

-- Returns the maximum number of defenders for a given army
maxDefenders :: Army -> Int
maxDefenders = min 2

-- Simulates n dice rolls
diceRolls :: Int -> Rand StdGen [DieValue]
diceRolls n = replicateM n die

-- Sorts a list of die values in decreasing order
sortDiceRolls :: [DieValue] -> [DieValue]
sortDiceRolls = sortBy (flip compare)

-- Returns the ordering for each die value matched in pairs
matchDiceRolls :: [DieValue] -> [DieValue] -> [Ordering]
matchDiceRolls = zipWith compare

-- Updates armies to reflect casualties
update :: Battlefield -> [Ordering] -> Battlefield
update (Battlefield as ds) xs = Battlefield (as - as_deaths) (ds - ds_deaths)
  where as_deaths = length . filter (/= GT) $ xs
        ds_deaths = length . filter (== GT) $ xs

-- Simulates a single battle between two opposing armies
battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  as_rolls <- sortDiceRolls <$> diceRolls (maxAttackers . attackers $ b)
  ds_rolls <- sortDiceRolls <$> diceRolls (maxDefenders . defenders $ b)
  return $ update b (matchDiceRolls as_rolls ds_rolls)


maxAttackersTest :: Bool
maxAttackersTest = and
  [
    0 == maxAttackers 1,
    1 == maxAttackers 2,
    2 == maxAttackers 3,
    3 == maxAttackers 4,
    3 == maxAttackers 10
  ]

maxDefendersTest :: Bool
maxDefendersTest = and
  [
    0 == maxDefenders 0,
    1 == maxDefenders 1,
    2 == maxDefenders 2,
    2 == maxDefenders 3
  ]

sortDiceRollsTest :: Bool
sortDiceRollsTest = and
  [
    []                    == sortDiceRolls [],
    [DV 1]                == sortDiceRolls [DV 1],
    fmap DV [6,5,4,3,2,1] == sortDiceRolls (fmap DV [1,2,3,4,5,6]),
    fmap DV [6,5,4,3,2,2] == sortDiceRolls (fmap DV [2,5,4,3,2,6])
  ]

matchDiceRollsTest :: Bool
matchDiceRollsTest = and
  [
    []         == matchDiceRolls [DV 1] [],
    [LT]       == matchDiceRolls [DV 1] [DV 5],
    [EQ]       == matchDiceRolls [DV 1] [DV 1, DV 4],
    [GT]       == matchDiceRolls [DV 5, DV 4] [DV 3],
    [EQ,GT,LT] == matchDiceRolls (fmap DV [1,4,3]) (fmap DV [1,2,6])
  ]


---------------------------------- Exercise 3 ----------------------------------

-- Returns whether or not an invasion has ended
hasInvasionEnded :: Battlefield -> Bool
hasInvasionEnded (Battlefield as ds) = ds == 0 || as < 2

-- Simulates an entire invasion attempt
invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM hasInvasionEnded battle


hasInvasionEndedTest :: Bool
hasInvasionEndedTest = and
  [
    False == hasInvasionEnded (Battlefield 5 9),
    False == hasInvasionEnded (Battlefield 2 8),
    True  == hasInvasionEnded (Battlefield 7 0),
    True  == hasInvasionEnded (Battlefield 0 0),
    True  == hasInvasionEnded (Battlefield 1 9)
  ]


---------------------------------- Exercise 4 ----------------------------------

-- Number of runs we want to simulate
runs :: Int
runs = 1000

-- Returns whether or not the attacker has won
hasAttackerWon :: Battlefield -> Bool
hasAttackerWon (Battlefield _ ds) = ds == 0

-- Simulates many invasions to compute the estimated probabilty that the
-- attacking army will destroy the defending army
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  simulations <- replicateM runs $ invade b
  let wins = length . filter hasAttackerWon $ simulations
  return $ fromIntegral wins / fromIntegral runs


---------------------------------- Exercise 5 ----------------------------------

data Casualties = OneA | TwoAs | OneD | TwoDs | OneA_OneD

-- Template to simulate all the possible outcomes of rolling dices
-- (To understand how to use it read README.md in this same directory)
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  a2 <- DV <$> [1..6]
  a3 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  d2 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1,a2,a3]
  let sortedDs = sortDiceRolls [d1,d2]
  return $ matchDiceRolls sortedAs sortedDs

-- Returns the probability of a given outcome in a battle 1vs1
battle1vs1Prob :: Casualties -> Double
battle1vs1Prob OneA      = 21/36
battle1vs1Prob TwoAs     = 0
battle1vs1Prob OneD      = 15/36
battle1vs1Prob TwoDs     = 0
battle1vs1Prob OneA_OneD = 0

-- Returns the probability of a given outcome in a battle 2vs1
battle2vs1Prob :: Casualties -> Double
battle2vs1Prob OneA      = 91/216
battle2vs1Prob TwoAs     = 0
battle2vs1Prob OneD      = 125/216
battle2vs1Prob TwoDs     = 0
battle2vs1Prob OneA_OneD = 0

-- Returns the probability of a given outcome in a battle 3vs1
battle3vs1Prob :: Casualties -> Double
battle3vs1Prob OneA      = 441/1296
battle3vs1Prob TwoAs     = 0
battle3vs1Prob OneD      = 855/1296
battle3vs1Prob TwoDs     = 0
battle3vs1Prob OneA_OneD = 0

-- Returns the probability of a given outcome in a battle 1vs2
battle1vs2Prob :: Casualties -> Double
battle1vs2Prob OneA      = 161/216
battle1vs2Prob TwoAs     = 0
battle1vs2Prob OneD      = 55/216
battle1vs2Prob TwoDs     = 0
battle1vs2Prob OneA_OneD = 0

-- Returns the probability of a given outcome in a battle 2vs2
battle2vs2Prob :: Casualties -> Double
battle2vs2Prob OneA      = 0
battle2vs2Prob TwoAs     = 581/1296
battle2vs2Prob OneD      = 0
battle2vs2Prob TwoDs     = 295/1296
battle2vs2Prob OneA_OneD = 420/1296

-- Returns the probability of a given outcome in a battle 3vs2
battle3vs2Prob :: Casualties -> Double
battle3vs2Prob OneA      = 0
battle3vs2Prob TwoAs     = 2275/7776
battle3vs2Prob OneD      = 0
battle3vs2Prob TwoDs     = 2890/7776
battle3vs2Prob OneA_OneD = 2611/7776

-- Returns the probability of a given outcome in a battle
battleProb :: Army -> Army -> Casualties -> Double
battleProb 1 1 = battle1vs1Prob
battleProb 2 1 = battle2vs1Prob
battleProb 3 1 = battle3vs1Prob
battleProb 1 2 = battle1vs2Prob
battleProb 2 2 = battle2vs2Prob
battleProb _ _ = battle3vs2Prob

-- Give a battlefield, returns another one with casualties taken into account
updateCasualties :: Battlefield -> Casualties -> Battlefield
updateCasualties (Battlefield as ds) OneA      = Battlefield (as-1) ds
updateCasualties (Battlefield as ds) TwoAs     = Battlefield (as-2) ds
updateCasualties (Battlefield as ds) OneD      = Battlefield as (ds-1)
updateCasualties (Battlefield as ds) TwoDs     = Battlefield as (ds-2)
updateCasualties (Battlefield as ds) OneA_OneD = Battlefield (as-1) (ds-1)

-- Lazy multiplication (do not evaluate second argument if first one is 0)
(.*) :: Double -> Double -> Double
0 .* _ = 0
x .* y = x * y

-- Returns the exact probability of success based on principles of probability
-- We use memoization in order to speed-up the computations
exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield _ 0) = 1
exactSuccessProb (Battlefield 0 _) = 0
exactSuccessProb (Battlefield 1 _) = 0
exactSuccessProb b@(Battlefield as ds) = sum probs
  where probs = do
          c <- [OneA, TwoAs, OneD, TwoDs, OneA_OneD]
          let nextBattleProb  = battleProb (maxAttackers as) (maxDefenders ds) c
          let nextBattlefield = updateCasualties b c
          return $ nextBattleProb .* memoizedProb nextBattlefield

-- Builds a table which is lazily computed using the function given as input
-- to compute the result of each entry
memoize :: (Army -> Army -> Double) -> [[Double]]
memoize f = map (\x -> map (f x) [0..]) [0..]

-- List of lists where the content is computed on demand (lazily)
-- It acts like a matrix where we access by index to obtain a result
table :: [[Double]]
table = memoize . curry $ exactSuccessProb . uncurry Battlefield

-- Given a battlefield, return the probability of success for the attacker
-- This function just retrieves the result from the memoization table
memoizedProb :: Battlefield -> Double
memoizedProb (Battlefield as ds) = table !! as !! ds
