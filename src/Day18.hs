module Day18 where

import Data.Array
import Data.Foldable

type Point = (Int, Int)
type Grid = Array Point Char

adj :: Grid -> Point -> [Point]
adj grid (y, x) = filter (inRange (bounds grid)) possible
 where
  possible =
    [ (y - 1, x), (y - 1, x - 1), (y - 1, x + 1)
    , (y + 1, x), (y + 1, x - 1), (y + 1, x + 1)
    , (y, x - 1), (y, x + 1)
    ]

updateAcre :: Grid -> Point -> Char
updateAcre grid p = case grid ! p of
  '.' -> if count '|' >= 3 then '|' else '.'
  '|' -> if count '#' >= 3 then '#' else '|'
  '#' -> if count '#' >= 1 && count '|' >= 1 then '#' else '.'
  c   -> error $ "Invalid acre type: " ++ show c
 where
  count ch = length $ filter (\p' -> grid ! p' == ch) $ adj grid p

turn :: Grid -> Grid
turn grid = array (bounds grid) $ do
  p <- range (bounds grid)
  return (p, updateAcre grid p)

buildGrid :: [String] -> Grid
buildGrid ls = array ((0, 0), (maxy, maxx)) idxs
 where
  maxy = length ls - 1
  maxx = length (head ls) - 1
  idxs = do
    (y, row) <- zip [0..] ls
    (x, c) <- zip [0..] row
    return ((y, x), c)

gridPretty :: Grid -> String
gridPretty grid = do
  y <- [0..maxy]
  x <- [0..maxx + 1]
  if x == maxx + 1
    then return '\n'
    else return $ grid ! (y, x)
 where
  (_, (maxy, maxx)) = bounds grid

countWoodenAndLumberyards :: Grid -> (Int, Int)
countWoodenAndLumberyards grid = foldl' acc (0, 0) (range (bounds grid))
 where
  acc (!ct, !cl) p | grid ! p == '|' = (ct + 1, cl)
                   | grid ! p == '#' = (ct, cl + 1)
                   | otherwise       = (ct, cl)

resourceValue :: Grid -> Int
resourceValue grid = a * b
 where
  (a, b) = countWoodenAndLumberyards grid

parseFile :: FilePath -> IO Grid
parseFile path = buildGrid . lines <$> readFile path

day18part1 :: FilePath -> IO ()
day18part1 path = do
  grid <- parseFile path
  putStrLn $ gridPretty grid
  let grid' = foldl' (\grid _ -> turn grid) grid [1..10]
  putStrLn $ gridPretty grid'
  print $ resourceValue grid'

day18part2 :: FilePath -> IO ()
day18part2 path = do
  grid <- parseFile path
  -- Starting from minute 431, every 28 minutes will have 0 net difference.
  let grid'     = foldl' (\grid _ -> turn grid) grid [1..431]
      endIdx    = 1000000000
      leftover  = (endIdx - 431) `div` 28
      nextStart = 431 + (28 * leftover)
      grid''    = foldl' (\grid _ -> turn grid) grid' [nextStart + 1..endIdx]
  print $ resourceValue grid''

day18main :: IO ()
day18main = day18part2 "resources/day18/input"
