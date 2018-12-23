module Day22 where

import Control.Monad.State
import Data.Array
import Data.Foldable
import Data.Hashable
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Debug.Trace

type Point = (Int, Int)
data Region = Rocky | Wet | Narrow deriving (Show, Eq)

geoIndexTable :: Point -> Int -> Point -> Array Point Int
geoIndexTable botRight depth target = table
 where
  bnds = ((0, 0), botRight)
  table = array bnds [(p, go p) | p <- range bnds]
  get p = if inRange bnds p then table ! p else 0

  go p | p == target = 0
  go (0, 0) = 0
  go (0, x) = x * 16807
  go (y, 0) = y * 48271
  go (y, x) = erosionLevel (get (y, x - 1)) depth
            * erosionLevel (get (y - 1, x)) depth

erosionLevel :: Int -> Int -> Int
erosionLevel geoIdx depth = (geoIdx + depth) `rem` 20183

region :: Int -> Region
region erosionLevel = case erosionLevel `rem` 3 of
  0 -> Rocky
  1 -> Wet
  2 -> Narrow
  _ -> error "Invalid region type"

regions :: Point -> Int -> Point -> Array Point Region
regions botRight depth target = region . flip erosionLevel depth
                            <$> geoIndexTable botRight depth target

sumRegions :: Int -> Array Point Int -> Int
sumRegions depth = foldl' (\sum gi -> sum + erosionLevel gi depth `rem` 3) 0

day22part1 :: Int -> Point -> Int
day22part1 depth target = sumRegions depth $ geoIndexTable target depth target

data Item = Gear | Torch | Neither deriving (Show, Eq, Generic)
instance Hashable Item

switches :: Item -> Region -> [Item]
switches item = \case
  Rocky  -> if item == Neither then [Gear, Torch] else [item]
  Wet    -> if item == Torch then [Gear, Neither] else [item]
  Narrow -> if item == Gear then [Torch, Neither] else [item]

adjs :: Point -> [Point]
adjs (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

type Cache = HM.HashMap (Point, Item) Int

dijkstra :: Array Point Region -> Point -> [(Point, Item)]
dijkstra regions target = prevsToList [] (Just (target, Torch))
                        $ go (HM.insert ((0, 0), Torch) 0 dists) prevs q
 where
  bnds = bounds regions
  items = [Gear, Torch, Neither]
  q = HS.fromList [(p, i) | p <- range bnds, i <- items]
  dists = HM.fromList [((p, i), maxBound :: Int) | p <- range bnds, i <- items]
  prevs = HM.fromList [((p, i), Nothing) | p <- range bnds, i <- items]
  prevsToList :: [(Point, Item)]
              -> Maybe (Point, Item)
              -> HM.HashMap (Point, Item) (Maybe (Point, Item))
              -> [(Point, Item)]
  prevsToList l Nothing _           = l
  prevsToList l (Just target) prevs = prevsToList (target:l) next prevs
   where next = join $ HM.lookup target prevs

  go :: HM.HashMap (Point, Item) Int
     -> HM.HashMap (Point, Item) (Maybe (Point, Item))
     -> HS.HashSet (Point, Item)
     -> HM.HashMap (Point, Item) (Maybe (Point, Item))
  go dists prevs set
    | pathFinished = prevs
    | otherwise    = go dists' prevs' (HS.delete u set)
   where
    u@(pos, item) = fst . minimumBy (comparing snd)
                  . fmap (\p -> (p, dists HM.! p))
                  $ HS.toList set
    pathFinished = HS.null set || u == (target, Torch)
    possMoves = appendTargetSwitch
              $ filter (inRange bnds) (adjs pos)
            >>= \p -> (p,) <$> switches item (regions ! p)
    (dists', prevs') = foldl' update (dists, prevs) possMoves

    neighborDist (_, i) (_, i') = if i == i' then 1 else 8
    -- Account for switching items at the target position to be Torch.
    appendTargetSwitch | pos == target && item /= Torch = ((pos, Torch):)
                       | otherwise                      = id
    update (!dists, !prevs) v
      | alt < dists HM.! v = (HM.insert v alt dists, HM.insert v (Just u) prevs)
      | otherwise          = (dists, prevs)
     where alt = dists HM.! u + neighborDist u v

sumPath :: [(Point, Item)] -> Int
sumPath []         = error "Empty path"
sumPath ((p, i):l) = go p i l
 where
  go _ _ [] = 0
  go p i ((p', i'):rest) = totalCost + go p' i' rest
   where
    itemCost = if i == i' then 0 else 7
    moveCost = if p == p' then 0 else 1
    totalCost = itemCost + moveCost

regionsPretty :: Array Point Region -> String
regionsPretty regions = do
  y <- [0..maxy]
  x <- [0..maxx + 1]
  if x == maxx + 1
    then return '\n'
    else return $ regionToChar $ regions ! (y, x)
 where
  (_, (maxy, maxx)) = bounds regions
  regionToChar Rocky  = '.'
  regionToChar Wet    = '='
  regionToChar Narrow = '|'

day22part2 :: Int -> Point -> IO ()
day22part2 depth target = do
  let rs   = regions (20, 20) depth (10, 10)
      path = dijkstra rs target
  putStrLn $ regionsPretty rs
  print path
  print $ sumPath path
