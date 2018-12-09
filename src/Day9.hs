module Day9 where

import Data.Foldable (foldl')
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.IntMap.Strict as IM

-- Note: Using Seq instead of lists for marbles is essential for
-- performance with larger numbers.
--
-- The bottleneck for lists seems to be with (++), not splitAt.
data Game = Game
  { _players       :: !(IM.IntMap Int)
  , _marbles       :: !(Seq Int)
  , _currentPlayer :: ![Int]
  , _marbleIndex   :: !Int
  , _nextMarble    :: !Int
  , _lastMarble    :: !Int
  }

mkGame :: Int -> Int -> Game
mkGame numPlayers lastMarble = Game
  { _players       = IM.fromList [(p, 0) | p <- players]
  , _marbles       = 0 :<| Empty
  , _currentPlayer = cycle players
  , _marbleIndex   = 0
  , _nextMarble    = 1
  , _lastMarble    = lastMarble
  }
 where
  players = [1..numPlayers]

insert :: Int -> a -> Seq a -> Seq a
insert i v l = (left :|> v) <> right
 where (left, right) = Seq.splitAt (i + 1) l

uncons :: Seq a -> (a, Seq a)
uncons (e :<| es) = (e, es)
uncons _          = error "Empty sequence"

delete :: Int -> Seq a -> (a, Seq a)
delete i l = (removed, left <> right')
 where
  (left, right) = Seq.splitAt i l
  (removed, right') = uncons right

ended :: Game -> Bool
ended g = _nextMarble g == _lastMarble g

turn :: Game -> Game
turn g@Game{ _currentPlayer = (p:ps) }
  | _nextMarble g `rem` 23 == 0 = g
    { _players       = IM.adjust (+ (_nextMarble g + removed)) p (_players g)
    , _marbles       = removedMarbles
    , _currentPlayer = ps
    , _marbleIndex   = sevenPrev
    , _nextMarble    = _nextMarble g + 1
    }
  | otherwise = g
    { _marbles       = insertedMarbles
    , _currentPlayer = ps
    , _marbleIndex   = nextIndex + 1
    , _nextMarble    = _nextMarble g + 1
    }
 where
  prev i len | i == 0    = len - 1
             | otherwise = i - 1
  next i len | i == len - 1 = 0
             | otherwise    = i + 1
  marblesLen = length $ _marbles g

  nextIndex = next (_marbleIndex g) marblesLen
  insertedMarbles = insert nextIndex (_nextMarble g) (_marbles g)

  sevenPrev = foldl' (\a _ -> prev a marblesLen) (_marbleIndex g) [1..7]
  (removed, removedMarbles) = delete sevenPrev (_marbles g)
turn _ = error "_currentPlayer should be infinite list"

highScore :: Game -> Int
highScore = go
 where
  score = maximum . fmap snd . IM.toList
  go !g
    | ended g   = score $ _players g'
    | otherwise = go g'
   where
    g' = turn g

day9 :: Int -- ^ Num players
     -> Int -- ^ Last marble points
     -> Int -- ^ High score
day9 numPlayers lastMarble = highScore $ mkGame numPlayers lastMarble
