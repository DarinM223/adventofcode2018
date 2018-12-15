module Day15 where

import Control.Monad (void)
import Data.Array
import Data.Foldable
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS

type Point = (Int, Int)
type Grid = Array (Int, Int) Char

data Location = Location
  { _coord :: Point
  , _steps :: Int
  } deriving (Show, Eq)
instance Ord Location where
  compare l1 l2 = comparing _steps l1 l2
               <> comparing (fst . _coord) l1 l2
               <> comparing (snd . _coord) l1 l2

data Unit = Unit
  { _type :: Char
  , _pos  :: Point
  , _hp   :: Int
  , _pow  :: Int
  } deriving (Show, Eq)

data Units = Units
  { _units        :: (M.Map (Int, Int) Unit)
  , _countElfs    :: Int
  , _countGoblins :: Int
  } deriving (Show, Eq)

possibleIndexes :: Point -> [(Int, Int)]
possibleIndexes (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

validPos :: Grid -> M.Map (Int, Int) Unit -> Point -> Bool
validPos grid map p =
  inRange (bounds grid) p && grid ! p /= '#' && not (M.member p map)

dist :: Grid -> Units -> Point -> Point -> Maybe Int
dist grid units p (y2, x2) = go HS.empty p
 where
  units' = M.delete p $ _units units
  go _ (y, x) | y == y2 && x == x2 = Just 0
  go prevs p
    | not $ validPos grid units' p = Nothing
    | HS.member p prevs            = Nothing
    | otherwise                    = dists
   where
    prevs' = HS.insert p prevs
    dists = fmap (+ 1) $ foldl' accDist Nothing (possibleIndexes p)
    accDist (Just minDist) = Just . maybe minDist (min minDist) . go prevs'
    accDist Nothing        = go prevs'

adjs :: Grid -> Units -> Unit -> Unit -> [Location]
adjs grid units curr = catMaybes
                     . fmap mapLoc
                     . filter (validPos grid (_units units))
                     . possibleIndexes
                     . _pos
 where
  mapLoc i = Location i <$> dist grid units (_pos curr) i

chooseTarget :: Grid -> Units -> Unit -> Maybe Location
chooseTarget grid units curr = case targets of
  [] -> Nothing
  _  -> Just $ minimum targets
 where
  unitList = filter ((/= _type curr) . _type) . M.elems $ _units units
  targets = unitList >>= adjs grid units curr

chooseMove :: Grid -> Units -> Unit -> Location -> Location
chooseMove grid units curr location = minimum moves
 where
  mapLoc i = Location i <$> dist grid units (_coord location) i
  moves = catMaybes
        . fmap mapLoc
        . filter (validPos grid (_units units))
        . possibleIndexes
        $ _pos curr

chooseEnemy :: Units -> Unit -> Maybe Unit
chooseEnemy units curr
  | null enemies = Nothing
  | otherwise    = Just $ minimumBy (comparing _hp) enemies
 where
  enemies = filter ((/= _type curr). _type)
          . catMaybes
          . fmap (flip M.lookup (_units units))
          . possibleIndexes
          $ _pos curr

move :: Units -> Point -> Unit -> Units
move units point curr = units { _units = update (_units units) }
 where
  curr' = curr { _pos = point }
  update = M.insert point curr' . M.delete (_pos curr)

attack :: Units -> Unit -> Unit -> Units
attack units attacker victim = units
  { _units        = update (_units units)
  , _countGoblins = updateCount 'G' (_countGoblins units)
  , _countElfs    = updateCount 'E' (_countElfs units)
  }
 where
  victim' = victim { _hp = _hp victim - _pow attacker }
  update units | _hp victim' <= 0 = M.delete (_pos victim') units
               | otherwise        = M.insert (_pos victim') victim' units
  updateCount ch count =
    if _type victim' == ch && _hp victim' <= 0 then count - 1 else count

turn :: Grid -> Units -> Point -> Units
turn _ units p | not $ M.member p (_units units) = units
turn grid units p = case chooseEnemy units curr of
  Just enemy -> attack units curr enemy
  Nothing    -> case chooseEnemy units' curr' of
    Just enemy -> attack units' curr' enemy
    Nothing    -> units'
 where
  curr = _units units M.! p
  moveUnit mv = (_coord mv, move units (_coord mv) curr)
  (p', units') = maybe (p, units) moveUnit
               . fmap (chooseMove grid units curr)
               $ chooseTarget grid units curr
  curr' = _units units' M.! p'

runTurn :: Grid -> Units -> Units
runTurn grid units = foldl'
  (\units unit -> turn grid units (_pos unit))
  units
  (M.elems $ _units units)

won :: Units -> Maybe Char
won units
  | _countGoblins units == 0 = Just 'E'
  | _countElfs units == 0    = Just 'G'
  | otherwise                = Nothing

countHitPoints :: Char -> Units -> Int
countHitPoints ch units = foldl' (\acc unit -> acc + _hp unit) 0
                        . filter ((== ch) . _type)
                        . M.elems
                        $ _units units

run :: Grid -> Units -> (Int, Int)
run = go 0
 where
  go :: Int -> Grid -> Units -> (Int, Int)
  go !turnNum grid units
    | Just ch <- won units = (turnNum - 1, countHitPoints ch units)
    | otherwise            = go (turnNum + 1) grid (runTurn grid units)

mkGrid :: [String] -> Grid
mkGrid ls = array ((0, 0), (maxy, maxx)) $ do
  (y, row) <- zip [0..] ls
  (x, ch)  <- zip [0..] row
  if ch /= '.' && ch /= '#'
    then return ((y, x), '.')
    else return ((y, x), ch)
 where
  maxy = length ls - 1
  maxx = length (head ls) - 1

mkUnits :: [String] -> Units
mkUnits ls = foldl' accRow (Units M.empty 0 0) (zip [0..] ls)
 where
  accRow units (y, row) = foldl' (acc y) units (zip [0..] row)
  acc y units (x, ch)
    | ch /= '.' && ch /= '#' = units
      { _units        = M.insert (y, x) unit (_units units)
      , _countGoblins = updateIfCh 'G' (_countGoblins units)
      , _countElfs    = updateIfCh 'E' (_countElfs units)
      }
    | otherwise = units
   where
     unit = Unit ch (y, x) 200 3
     updateIfCh ch' count = if ch == ch' then count + 1 else count

printState :: Grid -> Units -> IO ()
printState grid units = do
  forM_ [0..maxy] $ \y -> do
    forM_ [0..maxx] $ \x -> case M.lookup (y, x) (_units units) of
      Just unit -> putChar $ _type unit
      Nothing   -> putChar $ grid ! (y, x)
    putChar '\n'
 where
  (_, (maxy, maxx)) = bounds grid

parse :: FilePath -> IO (Grid, Units)
parse path = do
  ls <- lines <$> readFile path
  return (mkGrid ls, mkUnits ls)

day15part1 :: FilePath -> IO ()
day15part1 path = do
  (grid, units) <- parse path
  printState grid units
  print units
  void $ foldlM
    (\units _ -> do
      let units' = runTurn grid units
      printState grid units'
      print units'
      print $ countHitPoints 'E' units'
      return units')
    units
    [0..1000]
  {-let (turnNum, hitPoints) = run grid units-}
  {-putStrLn $ "Turn number: " ++ show turnNum-}
  {-putStrLn $ "Hit points: " ++ show hitPoints-}

day15main :: IO ()
{-day15main = day15part1 "mytest"-}
day15main = day15part1 "resources/day15/input"