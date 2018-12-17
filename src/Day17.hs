module Day17 where

import Control.Applicative (liftA2)
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
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
type Grid = Array (Int, Int) Char
type Waters = HM.HashMap (Int, Int) Char

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

newtype GridT m a = GridT { unGridT :: ReaderT Grid (StateT Waters m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Grid, MonadState Waters)

runGridT :: Grid -> Waters -> GridT m a -> m (a, Waters)
runGridT grid waters = flip runStateT waters . flip runReaderT grid . unGridT

getCell :: (MonadState Waters m, MonadReader Grid m)
        => (Int, Int) -> m (Maybe Char)
getCell p = do
  map <- get
  case HM.lookup p map of
    Just w  -> return $ Just w
    Nothing -> do
      grid <- ask
      let bnds = bounds grid
      if inRange bnds p
        then return $ Just $ grid ! p
        else return Nothing

setCell :: (MonadState Waters m) => (Int, Int) -> Char -> m ()
setCell p ch = modify' (HM.insert p ch)

mkGrid :: [Point] -> Grid
mkGrid points = initarr // changes
 where
  (miny, maxy, minx, maxx, changes) = foldl'
    buildGrid
    (maxBound :: Int, minBound :: Int, maxBound :: Int, minBound :: Int, [])
    points
  (minx', maxx') = (minx - 100, maxx + 100)
  initarr = array ((miny, minx'), (maxy, maxx')) $ do
    y <- [miny..maxy]
    x <- [minx'..maxx']
    return ((y, x), '.')
  xrange y x1 x2 = [(y, x) | x <- [x1..x2]]
  yrange x y1 y2 = [(y, x) | y <- [y1..y2]]
  buildGrid (!miny, !maxy, !minx, !maxx, l) = \case
    (Fixed y, Range x1 x2) ->
      ( min miny y
      , max maxy y
      , minimum [minx, x1, x2]
      , maximum [maxx, x1, x2]
      , l ++ zip (xrange y x1 x2) (repeat '#')
      )
    (Range y1 y2, Fixed x) ->
      ( minimum [miny, y1, y2]
      , maximum [maxy, y1, y2]
      , min minx x
      , max maxx x
      , l ++ zip (yrange x y1 y2) (repeat '#')
      )
    _ -> error "Both dimensions are ranges!"

fillWater :: Monad m => (Int, Int) -> GridT m Char
fillWater p@(y, x) = do
  ch <- getCell p
  case ch of
    Just '#' -> return '#'
    Nothing  -> return '|'
    _        -> fillWater (y + 1, x) >>= \case
      '|' -> '|' <$ setCell (y, x) '|'
      '#' -> fillRow (y, x)
      '~' -> fillRow (y, x)
      c   -> error $ "Invalid char: " ++ show c

fillRow :: Monad m => (Int, Int) -> GridT m Char
fillRow p@(y, _) = do
  leftBound@(_, lx) <- scanRow (subtract 1) (+ 1) p
  rightBound@(_, rx) <- scanRow (+ 1) (subtract 1) p
  liftA2 (,) (getCell leftBound) (getCell rightBound) >>= \case
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

scanRow :: Monad m
        => (Int -> Int) -> (Int -> Int) -> (Int, Int) -> GridT m (Int, Int)
scanRow f b (y, x) = liftA2 (,) (getCell (y, x)) (getCell (y + 1, x)) >>= \case
  (Just '#', _)        -> return (y, x)
  (Just '.', Just '.') -> return (y, x)
  (Just '|', Just '|') -> return (y, x)
  (Just _, _)          -> scanRow f b (y, f x)
  (Nothing, _)         -> return (y, b x)

fillParams :: (Char, (Int, Int), (Int, Int))
fillParams = undefined

parseFile :: FilePath -> IO [Point]
parseFile path = do
  ls <- lines <$> readFile path
  return $ fmap parseLine ls

printGrid :: (MonadIO m, MonadReader Grid m, MonadState Waters m) => m ()
printGrid = do
  ((miny, minx), (maxy, maxx)) <- bounds <$> ask
  forM_ [miny..maxy] $ \y -> do
    forM_ [minx..maxx] $ \x -> getCell (y, x) >>= liftIO . putChar . fromJust
    liftIO $ putChar '\n'

countWater :: (Monad m) => (Char -> Bool) -> GridT m Int
countWater fn = fmap snd $ flip runStateT 0 $ do
  ((miny, minx), (maxy, maxx)) <- bounds <$> lift ask
  forM_ [miny..maxy] $ \y ->
    forM_ [minx..maxx] $ \x -> lift (getCell (y, x)) >>= \case
      Just c | fn c -> modify' (+ 1)
      _             -> return ()

day17part1 :: FilePath -> IO ()
day17part1 path = do
  points <- parseFile path
  let grid           = mkGrid points
      ((miny, _), _) = bounds grid
      fn c           = c == '|' || c == '~'
  (water, _) <- runGridT grid HM.empty $
    fillWater (miny, 500) *> countWater fn <* printGrid
  print $ bounds grid
  print water

day17part2 :: FilePath -> IO ()
day17part2 path = do
  points <- parseFile path
  let grid           = mkGrid points
      ((miny, _), _) = bounds grid
  (water, _) <- runGridT grid HM.empty $
    fillWater (miny, 500) *> countWater (== '~') <* printGrid
  print $ bounds grid
  print water
