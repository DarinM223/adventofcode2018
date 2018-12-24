module Day23 where

import Data.Foldable
import Data.Ord (comparing)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
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

day23part2 :: FilePath -> IO ()
day23part2 path = do
  bots <- parseFile path
  let max = maximumBy (comparing _r) bots
  print max
