module Day22 where

import Control.Monad.State
import Control.Monad.ST
import Data.Array
import Data.Foldable
import Data.Hashable
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashPSQ as Q

type Point = (Int, Int)
type Index = (Point, Item)
data Region = Rocky | Wet | Narrow deriving (Show, Eq)
data Item = Gear | Torch | Neither deriving (Show, Eq, Ord, Generic)
instance Hashable Item

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

switches :: Item -> Region -> [Item]
switches item = \case
  Rocky  -> if item == Neither then [Gear, Torch] else [item]
  Wet    -> if item == Torch then [Gear, Neither] else [item]
  Narrow -> if item == Gear then [Torch, Neither] else [item]

-- | Account for switching items at the target position to be Torch.
appendTargetSwitch :: Index -> Index -> ([Index] -> [Index])
appendTargetSwitch (pos, item) (pos', item')
  | pos == pos' && item /= item' = ((pos, item'):)
  | otherwise                    = id

adjs :: Point -> [Point]
adjs (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

neighborDist :: Index -> Index -> Int
neighborDist (_, i) (_, i') = if i == i' then 1 else 8

manhattanDist :: Index -> Index -> Int
manhattanDist ((y, x), _) ((y', x'), _) = abs (y' - y) + abs (x' - x)

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
    possMoves = appendTargetSwitch u (target, Torch)
              $ filter (inRange bnds) (adjs pos)
            >>= \p -> (p,) <$> switches item (regions ! p)
    (dists', prevs') = foldl' update (dists, prevs) possMoves

    update (!dists, !prevs) v
      | alt < dists HM.! v = (HM.insert v alt dists, HM.insert v (Just u) prevs)
      | otherwise          = (dists, prevs)
     where alt = dists HM.! u + neighborDist u v

astar :: (Index -> Index -> Int)
      -> (Index -> Index -> Int)
      -> Array Point Region
      -> Index
      -> Index
      -> [Index]
astar dist heuristic regions start target = runST $ do
  let frontier = Q.fromList $ do
        p <- range bnds
        i <- items
        if (p, i) == start
          then return ((p, i), 0, ())
          else return ((p, i), maxBound :: Int, ())
  cameFrom <- HT.new :: ST s (HT.HashTable s Index (Maybe Index))
  costMap <- H.fromList
    [((p, i), maxBound :: Int) | p <- range bnds, i <- items]
    :: ST s (HT.HashTable s Index Int)
  H.insert costMap start 0

  let goNeighbor curr frontier neigh = do
        currCost <- fromJust <$> H.lookup costMap curr
        neighCost <- H.lookup costMap neigh
        let newCost = currCost + dist curr neigh
        if maybe True (> newCost) neighCost
          then do
            H.insert costMap neigh newCost
            H.insert cameFrom neigh (Just curr)
            let priority = newCost + heuristic neigh target
            return $ snd $ Q.alter (updatePriority priority) neigh frontier
          else return frontier
      go frontier = case Q.findMin frontier of
        Nothing                            -> return ()
        Just (curr, _, _) | curr == target -> return ()
        Just (curr, _, _)                  -> do
          let frontier' = Q.deleteMin frontier
          foldlM (goNeighbor curr) frontier' (possMoves curr) >>= go
  go frontier

  let prevsToList l Nothing _           = return l
      prevsToList l (Just target) prevs = do
        next <- join <$> H.lookup prevs target
        prevsToList (target:l) next prevs
  prevsToList [] (Just target) cameFrom
 where
  bnds = bounds regions
  items = [Gear, Torch, Neither]
  possMoves (pos, item) = appendTargetSwitch (pos, item) target
                        $ filter (inRange bnds) (adjs pos)
                      >>= \p -> (p,) <$> switches item (regions ! p)
  updatePriority _ Nothing        = ((), Nothing)
  updatePriority p' (Just (_, v)) = ((), Just (p', v))

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
  let rs   = regions (1000, 50) depth target
      path = astar neighborDist manhattanDist rs ((0, 0), Torch) (target, Torch)
  putStrLn $ regionsPretty rs
  print path
  print $ sumPath path
