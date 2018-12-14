module Day14 where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as S

digits :: Integral i => i -> Seq i
digits 0 = 0 :<| Empty
digits n = go n
 where
  go 0 = Empty
  go n = go (n `div` 10) :|> n `rem` 10

turn :: Seq Int -- ^ Full sequence
     -> Seq Int -- ^ Sequence for elf 1
     -> Seq Int -- ^ Sequence for elf 2
     -> (Seq Int, Seq Int, Seq Int) -- ^ Updated parameters
turn full elf1@(e1 :<| _) elf2@(e2 :<| _) =
  (full', moveForward full' (e1 + 1) elf1', moveForward full' (e2 + 1) elf2')
 where
  appendDigits = digits $ e1 + e2
  full' = full <> appendDigits
  elf1' = elf1 <> appendDigits
  elf2' = elf2 <> appendDigits
turn _ _ _ = error "Elf is empty"

moveForward :: Seq Int -> Int -> Seq Int -> Seq Int
moveForward _ 0 view = view
moveForward full amount view | amount >= S.length view =
  moveForward full (amount - S.length view) full
moveForward full amount (_ :<| rest) = moveForward full (amount - 1) rest
moveForward _ _ _                    = error "Elf can't move forward"

headSeq :: Seq a -> a
headSeq (e :<| _) = e
headSeq _         = error "Seq is empty"

updateUntilTenAfter :: Int -> Seq Int
updateUntilTenAfter amount =
  go (S.fromList [3, 7]) (S.fromList [3, 7]) (S.fromList [7])
 where
  go full e1 e2
    | S.length full >= amount + 10 = S.take 10 $ moveForward full amount full
    | otherwise                    = go full' e1' e2'
   where
    (full', e1', e2') = turn full e1 e2

updateUntilSequence :: Seq Int -> Int
updateUntilSequence pat =
  go (S.fromList [3, 7]) (S.fromList [3, 7]) (S.fromList [7])
 where
  go full e1 e2
    | pat == end  = fullLen - patLen
    | pat == end' = fullLen - patLen - 1
    | otherwise   = go full' e1' e2'
   where
    (full', e1', e2') = turn full e1 e2
    patLen = S.length pat
    fullLen = S.length full
    end = S.drop (fullLen - patLen) full
    end' = S.take patLen $ S.drop (fullLen - patLen - 1) full

day14part1 :: Int -> Seq Int
day14part1 = updateUntilTenAfter

day14part2 :: Int -> Int
day14part2 = updateUntilSequence . digits

-- | Takes ~16 seconds when -O2 is enabled, main is set to this
-- function, and you run it with `stack exec adventofcode2018-exe`
day14main :: IO ()
day14main = print $ day14part2 598701
