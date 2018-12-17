module Day17 where

import Control.Monad.State.Strict
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char.Lexer as L

data Coord = Fixed Int | Range Int Int
  deriving (Show, Eq)

type Point = (Coord, Coord) -- (y, x)
type GridMap = HM.HashMap (Int, Int) Char
data Grid = Grid
  { _minBounds :: (Int, Int)
  , _maxBounds :: (Int, Int)
  , _grid      :: GridMap
  } deriving (Show, Eq)

parseLine :: String -> Point
parseLine s = case runParser parse "" s of
  Left e  -> error $ errorBundlePretty e
  Right c -> c
 where
  parse :: Parsec Void String Point
  parse = parseXStarting <|> parseYStarting

  parseXStarting = flip (,)
               <$> (string "x=" *> parseCoord)
               <*> (string ", y=" *> parseCoord)
  parseYStarting = (,)
               <$> (string "y=" *> parseCoord)
               <*> (string ", x=" *> parseCoord)
  parseCoord = try (Range <$> L.decimal <*> (string ".." *> L.decimal))
           <|> (Fixed <$> L.decimal)

newtype GridT m a = GridT { unGridT :: StateT Grid m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Grid)

runGridT :: Grid -> GridT m a -> m (a, Grid)
runGridT grid = flip runStateT grid . unGridT

getCell :: MonadState Grid m => (Int, Int) -> m (Maybe Char)
getCell p = HM.lookup p <$> gets _grid

setCell :: (MonadState Grid m) => (Int, Int) -> Char -> m ()
setCell p ch = modify' (\grid -> grid { _grid = HM.insert p ch (_grid grid) })

mkGrid :: [Point] -> Grid
mkGrid points = Grid
  { _minBounds = (miny, minx')
  , _maxBounds = (maxy, maxx')
  , _grid      = foldl' (\grid (k, v) -> HM.insert k v grid) init changes
  }
 where
  (miny, maxy, minx, maxx, changes) = foldl'
    buildGrid
    (maxBound, minBound, maxBound, minBound, [])
    points
  (minx', maxx') = (minx - 10, maxx + 10)
  init = HM.fromList [((y, x), '.') | y <- [miny..maxy], x <- [minx'..maxx']]
  xrange y x1 x2 = [(y, x) | x <- [x1..x2]]
  yrange x y1 y2 = [(y, x) | y <- [y1..y2]]
  buildGrid (!miny, !maxy, !minx, !maxx, l) = \case
    (Fixed y, Range x1 x2) ->
      ( min miny y, max maxy y, minimum [minx, x1, x2], maximum [maxx, x1, x2]
      , l ++ zip (xrange y x1 x2) (repeat '#') )
    (Range y1 y2, Fixed x) ->
      ( minimum [miny, y1, y2], maximum [maxy, y1, y2], min minx x, max maxx x
      , l ++ zip (yrange x y1 y2) (repeat '#') )
    _ -> error "Both dimensions are ranges!"

fillWater :: Monad m => (Int, Int) -> GridT m Char
fillWater p@(y, x) = getCell p >>= \case
  Just '#' -> return '#'
  Nothing  -> return '|'
  _        -> fillWater (y + 1, x) >>= \case
    '|' -> '|' <$ setCell (y, x) '|'
    '#' -> fillRow (y, x)
    '~' -> fillRow (y, x)
    c   -> error $ "Invalid char: " ++ show c

fillRow :: Monad m => (Int, Int) -> GridT m Char
fillRow p@(y, _) = do
  grid <- gets _grid
  let leftBound@(_, lx)  = scanRow grid (subtract 1) p
      rightBound@(_, rx) = scanRow grid (+ 1) p
  case (HM.lookup leftBound grid, HM.lookup rightBound grid) of
    (Just '#', Just '#') -> do
      mapM_ (\x -> setCell (y, x) '~') [lx + 1..rx - 1]
      return '~'
    (Just leftCh, Just rightCh) -> do
      let leftX  = if leftCh == '.' then lx else lx + 1
          rightX = if rightCh == '.' then rx else rx - 1
      mapM_ (\x -> setCell (y, x) '|') [leftX..rightX]
      when (leftCh == '.') $ void $ fillWater (y, leftX)
      when (rightCh == '.') $ void $ fillWater (y, rightX)
      fromJust <$> getCell p
    _ -> error "This should not happen"

scanRow :: GridMap -> (Int -> Int) -> (Int, Int) -> (Int, Int)
scanRow grid f (y, x) =
  case (HM.lookup (y, x) grid, HM.lookup (y + 1, x) grid) of
    (Just '#', _)        -> (y, x)
    (Just '.', Just '.') -> (y, x)
    (Just '|', Just '|') -> (y, x)
    (Just _, _)          -> scanRow grid f (y, f x)
    (Nothing, _)         -> error "Grid X range too small"

printGrid :: (MonadIO m, MonadState Grid m) => m ()
printGrid = do
  (miny, minx) <- gets _minBounds
  (maxy, maxx) <- gets _maxBounds
  forM_ [miny..maxy] $ \y -> do
    forM_ [minx..maxx] $ \x -> getCell (y, x) >>= liftIO . putChar . fromJust
    liftIO $ putChar '\n'

countWater :: MonadState Grid m => (Char -> Bool) -> m Int
countWater fn = fmap snd $ flip runStateT 0 $ do
  (miny, minx) <- lift $ gets _minBounds
  (maxy, maxx) <- lift $ gets _maxBounds
  forM_ [(y, x) | y <- [miny..maxy], x <- [minx..maxx]] $ \p ->
    lift (getCell p) >>= maybe (pure ()) (\c -> when (fn c) (modify' (+ 1)))

parseFile :: FilePath -> IO [Point]
parseFile path = fmap parseLine . lines <$> readFile path

day17part1 :: FilePath -> IO ()
day17part1 path = do
  points <- parseFile path
  let grid      = mkGrid points
      (miny, _) = _minBounds grid
      fn c      = c == '|' || c == '~'
  (water, _) <- runGridT grid $
    fillWater (miny, 500) *> countWater fn <* printGrid
  print water

day17part2 :: FilePath -> IO ()
day17part2 path = do
  points <- parseFile path
  let grid      = mkGrid points
      (miny, _) = _minBounds grid
  (water, _) <- runGridT grid $ fillWater (miny, 500) *> countWater (== '~')
  print water
