module Day6 where

import Data.Foldable
import Data.List
import Data.Maybe (catMaybes)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char.Lexer as L

dist :: (Num a) => (a, a) -> (a, a) -> a
dist (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

parseLine :: String -> Maybe (Int, Int)
parseLine = eitherToMaybe . runParser parse ""
 where
  parse :: Parsec Void String (Int, Int)
  parse = (,) <$> L.decimal <*> ((string ", ") *> L.decimal)

closest :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
closest points point = fmap snd filteredPts
 where
  sortedPts = sortBy (\(d1, _) (d2, _) -> compare d1 d2)
            . fmap (\p -> (dist point p, p))
            $ points
  filteredPts = case sortedPts of
    (e@(dist, _):rest) -> e:filter (\(d, _) -> dist == d) rest
    []                 -> []

-- | Get (Min row, Max row, Min col, Max col)
edges :: [(Int, Int)] -> (Int, Int, Int, Int)
edges []         = error "Empty list"
edges ((r,c):es) = foldl' go (r,r,c,c) es
 where
  go (!minr, !maxr, !minc, !maxc) (r, c) =
    (min minr r, max maxr r, min minc c, max maxc c)

filterInf :: [(Int, Int)] -> [(Int, Int)]
filterInf l = filter (\p -> not $ p `elem` edgepoints) l
 where
  (minr, maxr, minc, maxc) = edges l
  indexes = [(r, minc) | r <- [minr-1..maxr+1]]
         ++ [(r, maxc) | r <- [minr-1..maxr+1]]
         ++ [(minr, c) | c <- [minc-1..maxc+1]]
         ++ [(maxr, c) | c <- [minc-1..maxc+1]]
  edgepoints = indexes >>= closest l

area :: [(Int, Int)] -> (Int, Int) -> Int
area l (a, b) = fst $ go (a, b) (a, b) HM.empty
 where
  go (r, c) (r', c') map
    | HM.member (r', c') map = (0, map)
    | partOfArea             = result
    | otherwise              = (0, map)
   where
    nearest = closest l (r', c')
    partOfArea = length nearest == 1 && head nearest == (r, c)
    possibilities = [(r' - 1, c'), (r' + 1, c'), (r', c' - 1), (r', c' + 1)]
    result = foldl'
      (\(c', map) (r'', c'') ->
        let (!dc, !map') = go (r, c) (r'', c'') map
        in (c' + dc, map'))
      (1, (HM.insert (r', c') True map))
      possibilities

readPoints path = catMaybes . fmap parseLine . lines <$> readFile path

day6part1 :: FilePath -> IO ()
day6part1 path = do
  points <- readPoints path
  let result = maximum . sort . fmap (area points) . filterInf $ points
  print result

day6part2 :: FilePath -> IO ()
day6part2 path = do
  points <- readPoints path
  undefined
