# Homework 12

This homework contains the following files:
* [Assignment](assignment.pdf)
* [Risk.hs (file to submit)](Risk.hs)

In this homework we build a simulator of the game of *Risk*. We define the following data types to represent the game:

```haskell
newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Show)
```

If you want to be able to run the code in [Risk.hs](Risk.hs), you must install [MonadRandom](http://hackage.haskell.org/package/MonadRandom) and [monad-loops](http://hackage.haskell.org/package/monad-loops) packages. To do that, run in the terminal:

```bash
cabal install MonadRandom
cabal install monad-loops
```

By the way, I found a very good [post](https://pbrisbin.com/posts/applicative_functors/) about when to use Applicative Functors and when to use Monads.

## Exercise 1

In this exercise we don't have to implement anything, just understand the rules of the game of *Risk*. It is important to read them thoroughly in order to comprehend what we will do next.

## Exercise 2

We implement the function `invade :: Battlefield -> Rand StdGen Battlefield` which simulates a single battle between two opposing armies. We have to do randomly roll dices, interpret the results and update the armies if there are casualties. Since there are many things to do, we will break this problem into many auxiliary functions.

First of all, we assume that each players always attacks or defends with the maximum number of units there are allowed. So we must write two functions that, given an `Army`, return what is the maximum number of units:

```haskell
maxAttackers :: Army -> Int
maxAttackers n = min 3 (n-1)

maxDefenders :: Army -> Int
maxDefenders = min 2
```

Next we have to simulate dice rolls. Since the number of dice rolls depends on the number of units, we pass that as a parameter. We use `replicateM :: Applicative m => Int -> m a -> m [a]` to execute an arbitrary number of dices:

```haskell
diceRolls :: Int -> Rand StdGen [DieValue]
diceRolls n = replicateM n die
```

Now we have to interpret the results. That means that we have to sort the dice rolls in decreasing order, match and compare them:

```haskell
sortDiceRolls :: [DieValue] -> [DieValue]
sortDiceRolls = sortBy (flip compare)

matchDiceRolls :: [DieValue] -> [DieValue] -> [Ordering]
matchDiceRolls = zipWith compare
```

The last step is to update both armies by reflecting their casualties. It is easy to count how many deaths each army has with the `[Ordering]` we have build in the previous function.

```haskell
update :: Battlefield -> [Ordering] -> Battlefield
update (Battlefield as ds) xs = Battlefield (as - as_deaths) (ds - ds_deaths)
  where as_deaths = length . filter (/= GT) $ xs
        ds_deaths = length . filter (== GT) $ xs
```

Finally, we use all the previous functions to build `battle`. It ends up being pretty readable using `do` notation. Note that it is easy to identify each of the steps that we described at the beginning (namely computing the number of units of each army, dicing rolls, sorting and matching them and updating both armies):

```haskell
battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  as_rolls <- sortDiceRolls <$> diceRolls (maxAttackers . attackers $ b)
  ds_rolls <- sortDiceRolls <$> diceRolls (maxDefenders . defenders $ b)
  return $ update b (matchDiceRolls as_rolls ds_rolls)
```

By the way, note that we now we can use the `>>=` (bind) operator to execute `battle` as many times as we want (both examples are equivalent):

```ghci
*Risk> evalRandIO $ return (Battlefield 5 5) >>= battle >>= battle >>= battle
Battlefield {attackers = 4, defenders = 1}
*Risk> evalRandIO $ battle (Battlefield 5 5) >>= battle >>= battle
Battlefield {attackers = 4, defenders = 1}
```

## Exercise 3

We have to implement `invade :: Battlefield -> Rand StdGen Battlefield` which simulates an entire invasion attempt, that is, it runs multiple battles until there are no defenders remaining or fewer than two attackers. This is similar to the `until :: (a -> Bool) -> (a -> a) -> a -> a` function for lists, so I looked if there was something equivalent for Monads and indeed there is. The function `iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a`does exactly the same. Note that I had to install the package `monad-loops` to import `import Control.Monad.Loops`. I think it is worth it to take a look at the source code to see how it works:

```haskell
-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f
```

Now, we just have to write a function `hasInvasionEnded :: Battlefield -> Bool` which tells us whether an invasion has ended or not and then the `invade :: Battlefield -> Rand StdGen Battlefield` implementation is obvious:

```haskell
hasInvasionEnded :: Battlefield -> Bool
hasInvasionEnded (Battlefield as ds) = ds == 0 || as < 2

invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM hasInvasionEnded battle
```

## Exercise 4

We have to implement the function `successProb :: Battlefield -> Rand StdGen Double` which simulates many invasions to compute the estimated probabilty that the attacking army will destroy the defending army. To solve this problem we must call `invade :: Battlefield -> Rand StdGen Battlefield` 1000 times and interpret the results.

First let's write two helpers functions: one to easily change the number of runs if we want to and another one to know whether or not the attacker has won:

```haskell
runs :: Int
runs = 1000

hasAttackerWon :: Battlefield -> Bool
hasAttackerWon (Battlefield _ ds) = ds == 0
```

Now we can easily implement `successProb`:

```haskell
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  simulations <- replicateM runs $ invade b
  let wins = length . filter hasAttackerWon $ simulations
  return $ fromIntegral wins / fromIntegral runs
```

By the way, if you want to try this function in the GHCi, use the following command:

```ghci
*Risk> evalRandIO . successProb $ Battlefield 20 20
```

## Exercise 5

This exercise was optional and it ended up being pretty time consuming but it's been totally worth it! I ended up learning a way of memoizing using Haskell's lazy evaluation which is something I did not expect to do at the beginning of this exercise.

The first challenge was the probability theory required to solve this problem. My knowledge of probabilty theory is rusty and I struggled a bit. I started by listing all the possible battles that may happen (1vs1, 2vs1, 3vs1, 1vs2, 2vs2, 3vs2) and computing the probabilities that the attacker had to succeed. 1vs1 was easy since there are only 36 possible outcomes, 2vs1 was harder but doable.

Using [this post](https://www.quora.com/Roll-two-fair-dice-Let-X-be-the-largest-of-the-two-faces-What-is-the-distribution-of-X) I managed to compute the 3vs1 case but when I reached the 3vs2 I was totally lost. There are five dice rolls (6<sup>5</sup> = 7776 possible outcomes) and you must take the two higher rolls of the attacker and match them against the two rolls of the defender. I had no clue of solving that in a theoretical way so I just ended up using the computer (and Haskell, obviously) to simulate all those possible outcomes and count how many times the attacker won.

Before continuing, note that in battles, there is always one or two deaths. If there is one death either one attacker or one defender will die, whereas if there are two deats two attackers, two defenders or one attacker and one defender die. Finally, don't get confused by the fact that a battle 1 vs 1 is represented by `Battlefield 2 1` since attackers must always leave at least one unit behind.

To represent the possible outcomes in terms of casualties that a battle can have, we create the `Casualties` data type:

```haskell
data Casualties = OneA | TwoAs | OneD | TwoDs | OneA_OneD
```

`OneA` means that one attacker dies, `TwoAs` that two attackers die, `OneD` that one defender dies, `TwoD` that two defenders die and `OneA_OneD` that one attacker and one defender die.

Next I will explain how I get the probabilities of each battle configuration. In the first battle I will explain in detail the process whereas in the remaining ones I will just paste the code that led me there.

### Battle 1 vs 1

In this battle there is one attacker and one defender. There are two dice rolls so 6<sup>2</sup> = 36 possible outcomes.

```haskell
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1]
  let sortedDs = sortDiceRolls [d1]
  return $ matchDiceRolls sortedAs sortedDs
```

If we execute the function `simulateDiceRolls` we get the list of lists `[[EQ],[LT],[LT],[LT],[LT],[LT],[GT],[EQ],[LT],[LT],[LT],[LT],[GT],[GT],[EQ],[LT],[LT],[LT],[GT],[GT],[GT],[EQ],[LT],[LT],[GT],[GT],[GT],[GT],[EQ],[LT],[GT],[GT],[GT],[GT],[GT],[EQ]]` which contains 36 lists. `GT` means that the die value of the attacker is greater than the one of the defender (so the defender dies), `EQ` means they are equal (so the attacker dies) and `LT` that the die value of the attacker is less than the defender (so the attacker dies).

From the result of `simulateDiceRolls` it is easy to deduce the number of times each success happened. From *GHCi* we can run the following two expressions to compute how many times attackers and defenders died.

```ghci
*Risk> length . filter (== 1) $ length . filter (/= GT) <$> simulateDiceRolls
21
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
15
```

That means that out of the 36 possible outcomes of the two dice rolls, 21 times the attacker will die and 15 times the defender will die.

```haskell
battle1vs1Prob :: Casualties -> Double
battle1vs1Prob OneA      = 21/36
battle1vs1Prob TwoAs     = 0
battle1vs1Prob OneD      = 15/36
battle1vs1Prob TwoDs     = 0
battle1vs1Prob OneA_OneD = 0
```

### Battle 2 vs 1

In this battle there are two attackers and one defender. There are three dice rolls so 6<sup>3</sup> = 216 possible outcomes.

```haskell
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  a2 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1,a2]
  let sortedDs = sortDiceRolls [d1]
  return $ matchDiceRolls sortedAs sortedDs
```

```ghci
*Risk> length . filter (== 1) $ length . filter (/= GT) <$> simulateDiceRolls
91
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
125
```

```haskell
battle2vs1Prob :: Casualties -> Double
battle2vs1Prob OneA      = 91/216
battle2vs1Prob TwoAs     = 0
battle2vs1Prob OneD      = 125/216
battle2vs1Prob TwoDs     = 0
battle2vs1Prob OneA_OneD = 0
```

### Battle 3 vs 1

In this battle there are three attackers and one defender. There are four dice rolls so 6<sup>4</sup> = 1296 possible outcomes.

```haskell
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  a2 <- DV <$> [1..6]
  a3 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1,a2,a3]
  let sortedDs = sortDiceRolls [d1]
  return $ matchDiceRolls sortedAs sortedDs
```

```ghci
*Risk> length . filter (== 1) $ length . filter (/= GT) <$> simulateDiceRolls
441
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
855
```

```haskell
battle3vs1Prob :: Casualties -> Double
battle3vs1Prob OneA      = 441/1296
battle3vs1Prob TwoAs     = 0
battle3vs1Prob OneD      = 855/1296
battle3vs1Prob TwoDs     = 0
battle3vs1Prob OneA_OneD = 0
```

### Battle 1 vs 2

In this battle there is one attacker and two defenders. There are three dice rolls so 6<sup>3</sup> = 216 possible outcomes.

```haskell
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  d2 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1]
  let sortedDs = sortDiceRolls [d1,d2]
  return $ matchDiceRolls sortedAs sortedDs
```

```ghci
*Risk> length . filter (== 1) $ length . filter (/= GT) <$> simulateDiceRolls
161
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
55
```

```haskell
battle1vs2Prob :: Casualties -> Double
battle1vs2Prob OneA      = 161/216
battle1vs2Prob TwoAs     = 0
battle1vs2Prob OneD      = 55/216
battle1vs2Prob TwoDs     = 0
battle1vs2Prob OneA_OneD = 0
```

### Battle 2 vs 2

In this battle there are two attackers and two defenders. There are four dice rolls so 6<sup>4</sup> = 1296 possible outcomes. Note that there will be two casualties in this battle: two attackers, two defenders or one attacker and one defender. For that reason, the *GHCi* code has an additional expression to compute how many times an attacker and defender die. Moreover, we use `filter (== 2)` for the cases where two units of the same army die.

```haskell
simulateDiceRolls :: [[Ordering]]
simulateDiceRolls = do
  a1 <- DV <$> [1..6]
  a2 <- DV <$> [1..6]
  d1 <- DV <$> [1..6]
  d2 <- DV <$> [1..6]
  let sortedAs = sortDiceRolls [a1,a2]
  let sortedDs = sortDiceRolls [d1,d2]
  return $ matchDiceRolls sortedAs sortedDs
```

```ghci
*Risk> length . filter (== 2) $ length . filter (/= GT) <$> simulateDiceRolls
581
*Risk> length . filter (== 2) $ length . filter (== GT) <$> simulateDiceRolls
295
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
420
```

```haskell
battle2vs2Prob :: Casualties -> Double
battle2vs2Prob OneA      = 0
battle2vs2Prob TwoAs     = 581/1296
battle2vs2Prob OneD      = 0
battle2vs2Prob TwoDs     = 295/1296
battle2vs2Prob OneA_OneD = 420/1296
```

### Battle 3 vs 2

In this battle there are three attackers and two defenders. There are five dice rolls so 6<sup>5</sup> = 7776 possible outcomes. Again, in this battle there are two casualties.

```haskell
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
```

```ghci
*Risk> length . filter (== 2) $ length . filter (/= GT) <$> simulateDiceRolls
2275
*Risk> length . filter (== 2) $ length . filter (== GT) <$> simulateDiceRolls
2890
*Risk> length . filter (== 1) $ length . filter (== GT) <$> simulateDiceRolls
2611
```

```haskell
battle3vs2Prob :: Casualties -> Double
battle3vs2Prob OneA      = 0
battle3vs2Prob TwoAs     = 2275/7776
battle3vs2Prob OneD      = 0
battle3vs2Prob TwoDs     = 2890/7776
battle3vs2Prob OneA_OneD = 2611/7776
```

Now that we have all the probabilites, we must build a function that given two armies, returns the probability of each outcome:

```haskell
battleProb :: Army -> Army -> Casualties -> Double
battleProb 1 1 = battle1vs1Prob
battleProb 2 1 = battle2vs1Prob
battleProb 3 1 = battle3vs1Prob
battleProb 1 2 = battle1vs2Prob
battleProb 2 2 = battle2vs2Prob
battleProb _ _ = battle3vs2Prob
```

Then we have to write a function that updates a battlefield given an outcome. For example, if the outcome is `OneA`, which means that an attacker died, we have to decrement the number of attackers by one. The same applies for the other outcomes:

```haskell
updateCasualties :: Battlefield -> Casualties -> Battlefield
updateCasualties (Battlefield as ds) OneA      = Battlefield (as-1) ds
updateCasualties (Battlefield as ds) TwoAs     = Battlefield (as-2) ds
updateCasualties (Battlefield as ds) OneD      = Battlefield as (ds-1)
updateCasualties (Battlefield as ds) TwoDs     = Battlefield as (ds-2)
updateCasualties (Battlefield as ds) OneA_OneD = Battlefield (as-1) (ds-1)
```

Now, we are ready to implement `exactSuccessProb :: Battlefield -> Double`. I think it's a good idea to think in terms of search spaces. Since in a battle there are attackers and defenders, we are in a 2D-space. We start with `Battlefield X Y` and after each battle we transition into others battlefields. Each of those successors has associated a probability of happening.


To compute the `exactSuccessProb` of a battlefield, we must compute the `exactSuccessProb` of each successor and multiply that by the probabilty of transitioning into that successor. In Haskell we would write: `successorProb * (exactSuccessProb successor)`. Note that this is a recursive definition, but it always ends since in each battle there is always one or two casualties. When there less than two attackers the attacker looses whereas when there are no defenders the attacker wins. Now we are ready to display the solution:

```haskell
(.*) :: Double -> Double -> Double
0 .* _ = 0
x .* y = x * y

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield _ 0) = 1
exactSuccessProb (Battlefield 0 _) = 0
exactSuccessProb (Battlefield 1 _) = 0
exactSuccessProb b@(Battlefield as ds) = sum probs
  where probs = do
          c <- [OneA, TwoAs, OneD, TwoDs, OneA_OneD]
          let nextBattlefieldProb = battleProb (maxAttackers as) (maxDefenders ds) c
          let nextBattlefield     = updateCasualties b c
          return $ nextBattlefieldProb .* exactSuccessProb nextBattlefield
```

Note that we have implemented a lazy multiplication operation to avoid making recursive calls when we multiply by 0 on the first argument. It makes no sense to keep computing a product that has a 0 inside! As we said, in base cases either the attacker won or lost, so the probabalities are 1 and 0. We use `do` notation instead of a comprehension list to iterate over all the possible outcomes in terms of casualties.

### Problems

After implementing this solution I used the simulation version to compare results. Instead of 1000 runs, I run it 100000 times and indeed they converged. So I was pretty certain it was well implemented. However, I realised the `exactSuccessProb` function was slow.

At first, I thought that it was intrinsic to the problem since we took into account all possible outcomes, including highly unlikely ones. For example, if there are 20 attackers and one defender, the defender might be a hero and kill all the attackers. This is highly unlikely, but if we want to compute the exact probability we must take that possibility into account. For that reason, the search space is huge and it's long to compute that. In fact, the [branching factor](https://en.wikipedia.org/wiki/Branching_factor) is either two or three so if we compute `Battlefield 20 20` we will generate about 3<sup>20</sup> calls.

Nevertheless, if you think about it, when we start with `Battlefield 20 20` there are not as many different battlefields as 3<sup>20</sup>. There is `Battlefield 20 19`, `Battlefied 19 20`, `Battlefield 19 19` up to a winning or losing battlefield. So there are only around 20*20 = 400 distinct battlefields! We can reuse the previous results to avoid making so many recursive calls.

### Memoization

To speed up the program we need to use [memoization](https://en.wikipedia.org/wiki/Memoization). In imperative languages we usually build a map and check whether or not we have already computed the result of the current state. However in Haskell we can use lazyness to implement a neat and unique way of doing memoization.

I used this [post](https://stackoverflow.com/a/5553390) to implement memoization with a lists of lists. The nice thing about doing it this way is that we don't have to think about how to construct the table: lazy evaluation ensures that values will be computed as needed. Note that this is not the most efficient way of implementing memoization since accessing elements in a list is linear in time. However, for this exercise it was not a problem.

The `exactSuccessProb` function looks almost the same as before, except we have changed `exactSuccessProb nextBattlefield` for `memoizedProb nextBattlefield`. Obviously we have also added the functions that allow us to memoize the results:

```haskell
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

memoize :: (Army -> Army -> Double) -> [[Double]]
memoize f = map (\x -> map (f x) [0..]) [0..]

table :: [[Double]]
table = memoize . curry $ exactSuccessProb . uncurry Battlefield

memoizedProb :: Battlefield -> Double
memoizedProb (Battlefield as ds) = table !! as !! ds
```

`table :: [[Double]]` can be seen as a matrix storing all the results we are memoizing. We access to the results by index, so for example `table !! 1 !! 1` stores the probability of success for `Battlefield 1 1`. What is nice about this solution is that we only computed what we need and that we don't have to build the whole matrix by ourselves. Building that matrix bottom-up is not obvious since we would have to think about how to build the graph of the recurrence.


Well, I guess it's time to finish this exercise, and also the course since that was the last exercise. I hope that you also had fun doing those assignments :)
