module Day20 where

import Data.Foldable
import Data.Sequence (Seq (..))
import Data.Void
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Data.Set as S

data Route = Route String | OrRoute [Regex] | EmptyRoute
  deriving (Show, Eq)
type Regex = Seq Route

parseRegex :: String -> Regex
parseRegex s = case runParser parse "" s of
  Left e  -> error $ errorBundlePretty e
  Right v -> v
 where
  parse :: Parsec Void String Regex
  parse = char '^' *> parseRoute <* char '$'

  parseOr = (:<|)
        <$> (OrRoute <$> (char '(' *> sepBy1 parseRoute (char '|') <* char ')'))
        <*> parseRoute

  parseRoute = parseOr
           <|> ((:<|) <$> (Route <$> some upperChar) <*> parseRoute)
           <|> return (EmptyRoute :<| Empty)

type Point = (Int, Int)
type Edge = ((Int, Int), (Int, Int))
type Edges = S.Set Edge

-- | Recursive descent version.
--
-- This is not performant enough for the input because of backtracking.
buildGrid :: Regex -> Edges -> Point -> Edges
buildGrid Empty edges _                     = edges
buildGrid (EmptyRoute :<| rest) edges p     = buildGrid rest edges p
buildGrid (OrRoute routes :<| rest) edges p =
  S.unions $ fmap (\r -> buildGrid (r <> rest) edges p) routes
buildGrid (Route s :<| rest) !edges !p = buildGrid rest edges' p'
 where (edges', p') = foldl' move (edges, p) s

-- | This one uses a set of points, making it more efficient.
buildGridSet :: Regex -> Edges -> S.Set Point -> (Edges, S.Set Point)
buildGridSet Empty edges ps                     = (edges, ps)
buildGridSet (EmptyRoute :<| rest) edges ps     = buildGridSet rest edges ps
buildGridSet (OrRoute routes :<| rest) edges ps = buildGridSet rest edges' ps'
 where
  results = fmap (\r -> buildGridSet r edges ps) routes
  edges' = S.unions $ fmap fst results
  ps' = S.unions $ fmap snd results
buildGridSet (Route s :<| rest) edges ps = buildGridSet rest edges' ps'
 where
  results = S.map (\p -> foldl' move (edges, p) s) ps
  edges' = S.unions $ S.map fst results
  ps' = S.map snd results

move :: (Edges, Point) -> Char -> (Edges, Point)
move (!edges, pos@(!y, !x)) c = (S.insert (pos, next) edges, next)
 where
  next = case c of
    'N' -> (y - 1, x)
    'S' -> (y + 1, x)
    'E' -> (y, x + 1)
    'W' -> (y, x - 1)
    _   -> error "Invalid direction"

bfs :: (acc -> Int -> acc) -> acc -> Edges -> (Int, Int) -> acc
bfs f acc graph p = go acc graph S.empty ((p, 0) :<| Empty)
 where
  go acc _ _ Empty = acc
  go acc graph set ((p, depth) :<| rest) =
    go (f acc depth) graph (S.insert p set) rest'
   where
    edges (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
    rest' = foldl' (\r e -> r :|> (e, depth + 1)) rest
          . filter (\e -> S.member (p, e) graph)
          . filter (`S.notMember` set)
          $ edges p

parseGrid :: String -> Edges
parseGrid = fst
          . (\r -> buildGridSet r S.empty (S.singleton (0, 0)))
          . parseRegex

parseFile :: FilePath -> IO Edges
parseFile path = do
  s <- filter (/= '\n') <$> readFile path
  let regex      = parseRegex s
      (graph, _) = buildGridSet regex S.empty (S.singleton (0, 0))
  return graph

day20part1 :: FilePath -> IO ()
day20part1 path = do
  graph <- parseFile path
  let longest = bfs max 0 graph (0, 0)
  print longest

day20part2 :: FilePath -> IO ()
day20part2 path = do
  graph <- parseFile path
  let count acc d = if d >= 1000 then acc + 1 else acc
      numGt1000   = bfs count 0 graph (0, 0)
  print numGt1000

day20main :: IO ()
day20main = day20part1 "resources/day20/input"
