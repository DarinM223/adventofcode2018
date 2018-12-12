module Day11 where

import Data.Array
import Data.Foldable

mkGrid :: [((Int, Int), Int)]
mkGrid = do
  x <- [1..300]
  y <- [1..300]
  return ((x, y), (x + 10) * y)

updateInputs :: Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
updateInputs input = fmap update
 where
  update ((x, y), p) = ((x, y), p''' - 5)
   where
    p' = p + input
    p'' = p' * (x + 10)
    p''' = (p'' `div` 100) `rem` 10

power :: Int -> (Int, Int) -> Array (Int,Int) Int -> Int
power size (x, y) arr = foldl'
  (\acc p -> acc + arr ! p)
  0
  [(x', y') | x' <- [x..x + (size - 1)], y' <- [y..y + (size - 1)]]

maxPower :: Int -> Array (Int, Int) Int -> ((Int, Int), Int)
maxPower size arr = foldl'
  go
  ((0, 0), 0)
  [(x, y) | x <- [1..300 - (size - 1)], y <- [1..300 - (size - 1)]]
 where
  go :: ((Int, Int), Int) -> (Int, Int) -> ((Int, Int), Int)
  go prev@(_, maxPower) (x, y)
    | power' > maxPower = ((x, y), power')
    | otherwise         = prev
   where
    power' = power size (x, y) arr

day11part1 :: Int -> ((Int, Int), Int)
day11part1 input = maxPower 3 arr
 where
  grid = updateInputs input mkGrid
  arr = array ((1, 1), (300, 300)) grid

data Best = Best
  { _bx   :: Int
  , _by   :: Int
  , _bs   :: Int
  , _best :: Int
  }

mkBest :: Best
mkBest = Best min min min min
 where min = -1000

day11part2 :: Int -> ((Int, Int, Int), Int)
day11part2 input = ((bx - bs + 1, by - bs + 1, bs), best)
 where
  Best bx by bs best = findBest sums

  sums = array ((0, 0), (300, 300)) $ do
    x <- [0..300]
    y <- [0..300]
    if x == 0 || y == 0
      then return ((y, x), 0)
      else return ((y, x), calcPower y x)
   where
    calcPower y x = p''
     where
      id = x + 10
      p = id * y + input
      p' = ((p * id) `div` 100) `mod` 10 - 5
      p'' = p' + sums ! (y-1, x) + sums ! (y, x-1) - sums ! (y-1, x-1)

  findBest sums = foldl' accBest mkBest
    [(s, y, x) | s <- [1..300], y <- [s..300], x <- [s..300]]
   where
    accBest prevBest (s, y, x)
      | total > _best prevBest = Best x y s total
      | otherwise              = prevBest
     where
      total =
        sums ! (y,x) - sums ! (y-s,x) - sums ! (y,x-s) + sums ! (y-s,x-s)
