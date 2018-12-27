module Day25 where

import Data.Foldable
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Point = (Int, Int, Int, Int)

parseLine :: String -> Point
parseLine s = case runParser parse "" s of
  Left e  -> error $ errorBundlePretty e
  Right v -> v
 where
  sc = L.space space1 empty empty
  int = L.signed sc L.decimal
  parse :: Parsec Void String (Int, Int, Int, Int)
  parse = (,,,)
        <$> int
        <*> (char ',' *> int)
        <*> (char ',' *> int)
        <*> (char ',' *> int)

manhattanDist :: Point -> Point -> Int
manhattanDist (x, y, z, w) (x', y', z', w') =
  abs (x' - x) + abs (y' - y) + abs (z' - z) + abs (w' - w)

parseFile :: FilePath -> IO [Point]
parseFile path = fmap parseLine . lines <$> readFile path

constellations :: [Point] -> [[Point]]
constellations = foldl' go []
 where
  fitsIntoConstellation ps p = canFit
   where canFit = any (<= 3) $ fmap (manhattanDist p) ps
  go cs p = if null updated then [p]:cs else (p:updated):cs'
   where
    check ps (updated, cs) = if fitsIntoConstellation ps p
      then (updated ++ ps, cs)
      else (updated, ps:cs)
    (updated, cs') = foldr check ([], []) cs

day25 :: FilePath -> IO ()
day25 path = do
  ps <- parseFile path
  let cs = constellations ps
  print $ length cs
