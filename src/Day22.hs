module Day22 where

import Control.Monad.State
import Control.Monad.ST
import Data.Array
import Data.Foldable
import Data.Hashable
import Data.Maybe (fromJust)
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashPSQ as Q

type Point = (Int, Int)
type Index = (Point, Item)
data Region = Rocky | Wet | Narrow deriving (Show, Eq)
data Item = Gear | Torch | Neither deriving (Show, Eq, Ord, Generic)
instance Hashable Item

geoIndexTable :: (Point, Point) -> Int -> Point -> Array Point Int
geoIndexTable bnds depth target = table
 where
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

regions :: (Point, Point) -> Int -> Point -> Array Point Region
regions bnds depth target = region . flip erosionLevel depth
                        <$> geoIndexTable bnds depth target

sumRegions :: Int -> Array Point Int -> Int
sumRegions depth = foldl' (\sum gi -> sum + erosionLevel gi depth `rem` 3) 0

day22part1 :: Int -> Point -> Int
day22part1 depth target = sumRegions depth
                        $ geoIndexTable ((0, 0), target) depth target

-- | In order to properly switch to an item, both your current
-- position and the next position have to be in a region that allows
-- you to use the item.
switches :: Item -> Region -> Region -> [Item]
switches item r r'
  | valid item = [item]
  | otherwise  = filter valid [Gear, Torch, Neither]
 where
  valid i = validRegion r i && validRegion r' i
  validRegion Rocky  = (/= Neither)
  validRegion Wet    = (/= Torch)
  validRegion Narrow = (/= Gear)

adjs :: Point -> [Point]
adjs (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

neighbors :: Array Point Region -> Index -> Index -> [Index]
neighbors regions (pos', item') (pos, item)
  | pos == pos' && item /= item' = (pos, item'):neighs
  | otherwise                    = neighs
 where
  neighs = filter (inRange (bounds regions)) (adjs pos)
       >>= \p -> (p,) <$> switches item (regions ! p) (regions ! pos)

neighborDist :: Index -> Index -> Int
neighborDist (p, i) (p', i') = itemCost + moveCost
 where
  itemCost = if i == i' then 0 else 7
  moveCost = if p == p' then 0 else 1

manhattanDist :: Index -> Index -> Int
manhattanDist ((y, x), _) ((y', x'), _) = abs (y' - y) + abs (x' - x)

heuristic :: Index -> Index -> Int
heuristic a@(_, i) b = manhattanDist a b + if i /= Torch then 7 else 0

astar :: forall i. (Ord i, Hashable i)
      => (i -> i -> Int) -- ^ Distance between adjacent vertexes
      -> (i -> i -> Int) -- ^ Heuristic function
      -> (i -> [i])      -- ^ Returns neighbors for vertex
      -> [i]             -- ^ List of all indexes in graph
      -> i               -- ^ Start index
      -> i               -- ^ End index
      -> [i]             -- ^ Path from start index to end index
astar dist heuristic neighbors indexes start target = runST $ do
  let init i   = if i == start then (i, 0, ()) else (i, maxBound, ())
      frontier = Q.fromList $ fmap init indexes
  cameFrom <- HT.new
  (costMap :: HT.HashTable s i Int) <- H.fromList ((, maxBound) <$> indexes)
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
          foldlM (goNeighbor curr) frontier' (neighbors curr) >>= go
  go frontier
  prevsToList [] (Just target) cameFrom
 where
  updatePriority _ Nothing        = ((), Nothing)
  updatePriority p' (Just (_, v)) = ((), Just (p', v))
  prevsToList l Nothing _           = return l
  prevsToList l (Just target) prevs = do
    next <- join <$> H.lookup prevs target
    prevsToList (target:l) next prevs

sumPath :: [(Point, Item)] -> Int
sumPath []    = error "Empty path"
sumPath (p:l) = go p l
 where
  go _ []        = 0
  go p (p':rest) = neighborDist p p' + go p' rest

regionsPretty :: [Index] -> Array Point Region -> String
regionsPretty path regions = do
  y <- [0..maxy]
  x <- [0..maxx + 1]
  if x == maxx + 1
    then return '\n'
    else if HM.member (y, x) pathMap
      then return $ itemToChar $ pathMap HM.! (y, x)
      else return $ regionToChar $ regions ! (y, x)
 where
  (_, (maxy, maxx)) = bounds regions
  pathMap = foldl' (\map (p, i) -> HM.insert p i map) HM.empty path
  regionToChar Rocky  = '.'
  regionToChar Wet    = '='
  regionToChar Narrow = '|'
  itemToChar Gear    = 'G'
  itemToChar Torch   = 'T'
  itemToChar Neither = 'N'

day22part2 :: IO ()
day22part2 = do
  let
    bnds    = ((0, 0), (800, 50))
    depth   = 4848
    target  = (700, 15)
    rs      = regions bnds depth target
    target' = (target, Torch)
    neighs  = neighbors rs target'
    idxs    = [(p, i) | p <- range bnds, i <- [Gear, Torch, Neither]]
    path    = astar neighborDist heuristic neighs idxs ((0, 0), Torch) target'
  putStrLn $ regionsPretty path rs
  print $ sumPath path
