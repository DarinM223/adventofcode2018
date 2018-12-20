module Day3 where

import Conduit
import Data.Foldable (foldl')
import Data.Hashable
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

newtype Row = Row Int deriving (Show, Eq, Num, Ord, Enum, Hashable)
newtype Col = Col Int deriving (Show, Eq, Num, Ord, Enum, Hashable)
newtype ID = ID { unID :: Int } deriving (Show, Eq, Num, Ord, Hashable)

data Value = Value
  { _firstID :: Maybe ID
  , _count   :: Int
  } deriving (Show, Eq)

emptyValue :: Value
emptyValue = Value Nothing 0

incValue :: ID -> Value -> Value
incValue id (Value Nothing count)  = Value (Just id) (count + 1)
incValue _ (Value (Just id) count) = Value (Just id) (count + 1)

type Grid = HM.HashMap (Row, Col) Value

finalGrid :: Grid
finalGrid = mkGrid 0 999

testGrid :: Grid
testGrid = mkGrid 0 9

mkGrid :: Int -> Int -> Grid
mkGrid start end = HM.fromList $ do
  x <- [Row start..Row end]
  y <- [Col start..Col end]
  return ((x, y), emptyValue)

data Region = Region
  { _id     :: ID
  , _col    :: Col -- ^ From left
  , _row    :: Row -- ^ From top
  , _width  :: Col
  , _height :: Row
  } deriving (Show, Eq)

applyRegion :: Region -> Grid -> Grid
applyRegion region grid = foldl'
  (flip (HM.adjust (incValue (_id region))))
  grid
  (indexes region)

indexes :: Region -> [(Row, Col)]
indexes region = do
  r <- [_row region.._row region + _height region - 1]
  c <- [_col region.._col region + _width region - 1]
  return (r, c)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

countOverlaps :: Grid -> Int
countOverlaps = length . filter ((> 1) . _count . snd) . HM.toList

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

parseLine :: T.Text -> Maybe Region
parseLine = eitherToMaybe . runParser parse ""
 where
  parse = Region
      <$> (ID <$> (char '#' *> L.decimal))
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
  accum grid = maybe grid (`applyRegion` grid) . parseLine

noOverlaps :: Grid -> Region -> Bool
noOverlaps grid = foldr checkIndex True . indexes
 where
  checkIndex _ False = False
  checkIndex i _     = case HM.lookup i grid of
    Just v -> _count v == 1
    _      -> False

candidates :: Grid -> [Int]
candidates = IS.toList . IS.fromList
           . mapMaybe (fmap unID . _firstID . snd)
           . filter ((== 1) . _count . snd)
           . HM.toList

day3part2 :: FilePath -> IO [Region]
day3part2 path = fmap check $ runConduitRes
  $  CB.sourceFile path
  .| CT.decode CT.utf8
  .| CT.lines
  .| CL.fold accum (finalGrid, IM.empty)
 where
  accum (!grid, !regions) line = case parseLine line of
    Just region -> ( applyRegion region grid
                   , IM.insert (unID $ _id region) region regions )
    Nothing -> (grid, regions)
  check (grid, regions) = filter (noOverlaps grid)
                        . mapMaybe (`IM.lookup` regions)
                        $ candidates grid
