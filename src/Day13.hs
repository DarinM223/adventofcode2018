module Day13 where

import Control.Monad (guard)
import Data.Array
import Data.Foldable
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)
type Grid = Array (Int, Int) Char
type Carts = Set Cart

data Direction = LeftDir | RightDir | UpDir | DownDir deriving (Show, Eq)
data TurnType = TurnLeft | GoStraight | TurnRight deriving (Show, Eq)

data Cart = Cart
  { _position  :: (Int, Int)
  , _direction :: Direction
  , _turn      :: TurnType
  } deriving (Show, Eq)

turn :: TurnType -> Direction -> Direction
turn TurnLeft LeftDir   = DownDir
turn TurnLeft RightDir  = UpDir
turn TurnLeft UpDir     = LeftDir
turn TurnLeft DownDir   = RightDir
turn GoStraight dir     = dir
turn TurnRight LeftDir  = UpDir
turn TurnRight RightDir = DownDir
turn TurnRight UpDir    = RightDir
turn TurnRight DownDir  = LeftDir

turnLeftCurve :: Direction -> Direction
turnLeftCurve UpDir    = RightDir
turnLeftCurve DownDir  = LeftDir
turnLeftCurve LeftDir  = DownDir
turnLeftCurve RightDir = UpDir

turnRightCurve :: Direction -> Direction
turnRightCurve UpDir    = LeftDir
turnRightCurve DownDir  = RightDir
turnRightCurve LeftDir  = UpDir
turnRightCurve RightDir = DownDir

nextTurnType :: TurnType -> TurnType
nextTurnType TurnLeft   = GoStraight
nextTurnType GoStraight = TurnRight
nextTurnType TurnRight  = TurnLeft

instance Ord Cart where
  compare c1 c2 = comparing (fst . _position) c1 c2
               <> comparing (snd . _position) c1 c2

buildGridLine :: Int -> String -> ([((Int, Int), Char)], [Cart])
buildGridLine y s = (cells, carts)
 where
  isCart c = c `elem` ("<>^v" :: String)
  cells = [((y, x), replace c) | (x, c) <- zip [0..] s]
  carts = do
    (x, c) <- zip [0..] s
    guard (isCart c)
    return $ Cart (y, x) (direction c) TurnLeft
  replace c | c `elem` ("><" :: String) = '-'
            | c `elem` ("^v" :: String) = '|'
            | otherwise                 = c
  direction = \case
    '<' -> LeftDir
    '>' -> RightDir
    '^' -> UpDir
    'v' -> DownDir
    _   -> error "Invalid direction"

buildGrid :: [String] -> (Grid, Carts)
buildGrid ls = (array ((0, 0), (maxy, maxx)) cells, S.fromList carts)
 where
  maxy = length ls - 1
  maxx = length (head ls) - 1
  (cells, carts) = foldl' accLine ([], []) (zip [0..] ls)
  accLine (cells, carts) (y, line) = (cells ++ cells', carts ++ carts')
   where (cells', carts') = buildGridLine y line

updateCarts :: Bool -> Grid -> Carts -> Either Point Carts
updateCarts recover grid = go S.empty
 where
  go finished unfinished
    | S.null unfinished    = Right finished
    | checkCollision cart' = if recover
      then go (deleteCart nextPos finished) (deleteCart nextPos rest)
      else Left nextPos
    | otherwise = go (S.insert cart' finished) rest
   where
    (Cart (y, x) dir turnType, rest) = S.deleteFindMin unfinished
    nextPos = case dir of
      LeftDir  -> (y, x - 1)
      RightDir -> (y, x + 1)
      UpDir    -> (y - 1, x)
      DownDir  -> (y + 1, x)
    (nextDir, nextTurn) = case grid ! nextPos of
      '+'  -> (turn turnType dir, nextTurnType turnType)
      '/'  -> (turnLeftCurve dir, turnType)
      '\\' -> (turnRightCurve dir, turnType)
      _    -> (dir, turnType)
    cart' = Cart nextPos nextDir nextTurn
    checkCollision cart = S.member cart rest || S.member cart finished

getCart :: Carts -> Point -> Maybe Cart
getCart carts p = case S.lookupIndex (Cart p undefined undefined) carts of
  Just i  -> Just $ S.elemAt i carts
  Nothing -> Nothing

deleteCart :: Point -> Carts -> Carts
deleteCart pos = S.delete (Cart pos undefined undefined )

cartChar :: Direction -> Char
cartChar LeftDir  = '<'
cartChar RightDir = '>'
cartChar UpDir    = '^'
cartChar DownDir  = 'v'

updateForever :: Grid -> Carts -> Either Point Carts
updateForever grid carts = updateCarts False grid carts >>= updateForever grid

updateUntilOneLeft :: Grid -> Carts -> Either Point Carts
updateUntilOneLeft grid carts = case updateCarts True grid carts of
  Left _                            -> error "This should not happen"
  Right carts' | S.size carts' == 1 -> Right carts'
  Right carts'                      -> updateUntilOneLeft grid carts'

printState :: Grid -> Carts -> IO ()
printState grid carts =
  forM_ [0..endY] $ \y -> do
    forM_ [0..endX] $ \x -> case getCart carts (y, x) of
      Just cart -> putChar $ cartChar $ _direction cart
      Nothing   -> putChar $ grid ! (y, x)
    putChar '\n'
 where
  (_, (endY, endX)) = bounds grid

fromLeft :: Either e a -> e
fromLeft (Left e) = e
fromLeft _        = error "Expected left"

fromRight :: Either e a -> a
fromRight (Right v) = v
fromRight _         = error "Expected right"

day13part1 :: FilePath -> IO ()
day13part1 path = do
  ls <- lines <$> readFile path
  let (grid, carts) = buildGrid ls
      collision     = fromLeft $ updateForever grid carts
  print collision

day13part2 :: FilePath -> IO ()
day13part2 path = do
  ls <- lines <$> readFile path
  let (grid, carts) = buildGrid ls
      carts'        = fromRight $ updateUntilOneLeft grid carts
      cart          = S.elemAt 0 carts'
  print cart
