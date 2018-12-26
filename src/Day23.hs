module Day23 where

import Data.Foldable
import Data.Ord (Down (Down), comparing)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Point = (Int, Int, Int)

data Nanobot = Nanobot
  { _pos :: Point
  , _r   :: Int
  } deriving (Show, Eq)

parseLine :: String -> Nanobot
parseLine s = case runParser parse "" s of
  Left e  -> error $ errorBundlePretty e
  Right v -> v
 where
  sc :: Parsec Void String ()
  sc = L.space space1 empty empty
  int = L.signed sc L.decimal

  parse = Nanobot
      <$> (string "pos=<" *> parsePos <* string ">,")
      <*> (sc *> string "r=" *> int)
  parsePos = (,,) <$> int <*> (char ',' *> int) <*> (char ',' *> int)

parseFile :: FilePath -> IO [Nanobot]
parseFile path = do
  ls <- lines <$> readFile path
  return $ fmap parseLine ls

dist :: Point -> Point -> Int
dist (x, y, z) (x', y', z') = abs (x' - x) + abs (y' - y) + abs (z' - z)

day23part1 :: FilePath -> IO ()
day23part1 path = do
  bots <- parseFile path
  let max         = maximumBy (comparing _r) bots
      inRange bot = dist (_pos bot) (_pos max) <= _r max
      botsInRange = filter inRange bots
  print $ length botsInRange

data SearchCube = SearchCube
  { _botLeft    :: (Int, Int, Int)
  , _length     :: Int -- ^ as the closest power of two
  , _numBots    :: Int
  , _originDist :: Int
  } deriving (Show, Eq)
instance Ord SearchCube where
  compare c1 c2 = comparing (Down . _numBots) c1 c2
               <> comparing _originDist c1 c2
               <> comparing _length c1 c2

cubeBounds :: [Nanobot] -> (Int, Int, Int, Int, Int, Int)
cubeBounds = foldl' acc
  (maxBound, minBound, maxBound, minBound, maxBound, minBound)
 where
  acc (!minx, !maxx, !miny, !maxy, !minz, !maxz) (Nanobot (x, y, z) r) =
    ( min minx (x - r), max maxx (x + r)
    , min miny (y - r), max maxy (y + r)
    , min minz (z - r), max maxz (z + r) )

countBots :: [Nanobot] -> SearchCube -> Int
countBots bots c = foldl' accBots 0 bots
 where
  (x1, y1, z1) = _botLeft c
  (x2, y2, z2) = (x1 + _length c - 1, y1 + _length c - 1, z1 + _length c - 1)
  rangeDist x lo hi | x < lo    = lo - x
                    | x > hi    = x - hi
                    | otherwise = 0
  accBots count (Nanobot (x, y, z) r) = if d <= r then count + 1 else count
   where d = rangeDist x x1 x2 + rangeDist y y1 y2 + rangeDist z z1 z2

initLength :: (Int, Int, Int, Int, Int, Int) -> Int
initLength (x, maxx, y, maxy, z, maxz) = go 1
 where
  go l = if x + l < maxx || y + l < maxy || z + l < maxz
    then go (l * 2)
    else l

mkSearchCube :: [Nanobot] -> Int -> Int -> Int -> Int -> SearchCube
mkSearchCube bots minx miny minz length = cube
 where
  cube = SearchCube
    { _botLeft    = (minx, miny, minz)
    , _length     = length
    , _numBots    = countBots bots cube
    , _originDist = abs minx + abs miny + abs minz
    }

findSmallestDist :: [Nanobot] -> Int
findSmallestDist bots = go $ S.singleton init
 where
  bounds@(minx, _, miny, _, minz, _) = cubeBounds bots
  init = mkSearchCube bots minx miny minz $ initLength bounds
  go set
    | _length cube == 1 = _originDist cube
    | otherwise         = go set''
   where
    (cube, set') = S.deleteFindMin set
    newside = _length cube `div` 2
    (x, y, z) = _botLeft cube
    subCubes = do
      x' <- [x, x + newside]
      y' <- [y, y + newside]
      z' <- [z, z + newside]
      return $ mkSearchCube bots x' y' z' newside
    set'' = foldl' (flip S.insert) set' subCubes

-- | Explanation here:
-- https://raw.githack.com/ypsu/experiments/master/aoc2018day23/vis.html
day23part2 :: FilePath -> IO ()
day23part2 path = do
  bots <- parseFile path
  print $ findSmallestDist bots
