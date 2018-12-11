{-# LANGUAGE ScopedTypeVariables #-}

module Day11 where

import Control.Monad.State
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Foldable
import qualified Data.Array.MArray as A

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

-- Only way to solve this is with leetcode BS
partialSumBS :: (A.MArray a Int m) => Int -> a (Int, Int) Int -> m Best
partialSumBS input sumArr = do
  forM_ [(x, y) | x <- [1..300], y <- [1..300]] $ \(x, y) -> do
    let id = x + 10
        p  = id * y + input
        p' = ((p * id) `div` 100) `mod` 10 - 5
    prevY <- A.readArray sumArr (y - 1, x)
    prevX <- A.readArray sumArr (y, x - 1)
    prevBoth <- A.readArray sumArr (y - 1, x - 1)
    A.writeArray sumArr (y, x) (p' + prevY + prevX - prevBoth)
  (_, best) <- flip runStateT mkBest $ do
    forM_ [(s, y, x) | s <- [1..300], y <- [s..300], x <- [s..300]] $ \(s, y, x) -> do
      value <- lift $ A.readArray sumArr (y, x)
      prevY <- lift $ A.readArray sumArr (y - s, x)
      prevX <- lift $ A.readArray sumArr (y, x - s)
      prevBoth <- lift $ A.readArray sumArr (y - s, x - s)
      let total = value - prevY - prevX + prevBoth
      best <- gets _best
      when (total > best) $ put $ Best x y s total
  return best

day11part2 :: Int -> ((Int, Int, Int), Int)
day11part2 input = runST go
 where
  go :: forall s. ST s ((Int, Int, Int), Int)
  go = do
    (arr' :: STArray s (Int, Int) Int) <- A.newArray ((0, 0), (300, 300)) 0
    Best bx by bs best <- partialSumBS input arr'
    return ((bx - bs + 1, by - bs + 1, bs), best)
