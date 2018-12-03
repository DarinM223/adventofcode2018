module Day3 where

import Conduit
import Data.Foldable (foldl')
import Data.Hashable
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

newtype Row = Row Int deriving (Show, Eq, Num, Ord, Enum, Hashable)
newtype Col = Col Int deriving (Show, Eq, Num, Ord, Enum, Hashable)

type Grid = HM.HashMap (Row, Col) Int

finalGrid :: Grid
finalGrid = mkGrid 0 999

testGrid :: Grid
testGrid = mkGrid 0 9

mkGrid :: Int -> Int -> Grid
mkGrid start end = HM.fromList $ do
  x <- [Row start..Row end]
  y <- [Col start..Col end]
  return ((x, y), 0)

data Region = Region
  { _id     :: Int
  , _col    :: Col -- ^ From left
  , _row    :: Row -- ^ From top
  , _width  :: Col
  , _height :: Row
  } deriving (Show, Eq)

applyRegion :: Region -> Grid -> Grid
applyRegion region grid =
  foldl' (\grid i -> HM.adjust (+ 1) i grid) grid indexes
 where
  indexes = do
    r <- [_row region.._row region + _height region - 1]
    c <- [_col region.._col region + _width region - 1]
    return (r, c)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

countOverlaps :: Grid -> Int
countOverlaps = length . filter ((> 1) . snd) . HM.toList

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

parseLine :: T.Text -> Maybe Region
parseLine = eitherToMaybe . runParser parse ""
 where
  parse = Region
      <$> (char '#' *> L.decimal)
      <*> (Col <$> (sc *> char '@' *> sc *> L.decimal))
      <*> (Row <$> (char ',' *> L.decimal))
      <*> (Col <$> (char ':' *> sc *> L.decimal))
      <*> (Row <$> (char 'x' *> L.decimal))

day3part1 :: FilePath -> IO Int
day3part1 path = fmap countOverlaps $ runConduitRes
  $  CB.sourceFile path
  .| CT.decode CT.utf8
  .| CT.lines
  .| CL.fold accum finalGrid
 where
  accum grid = maybe grid (flip applyRegion grid) . parseLine
