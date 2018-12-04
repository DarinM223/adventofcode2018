module Day4 where

import Conduit
import Data.Foldable (foldl', maximumBy)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data Action = BeginShift Int | FallsAsleep | WakesUp
  deriving (Show, Eq)

data Timestamp = Timestamp
  { _year   :: Int
  , _month  :: Int
  , _day    :: Int
  , _hour   :: Int
  , _minute :: Int
  , _action :: Action
  } deriving (Show, Eq)

instance Ord Timestamp where
  compare t1 t2 = compare (_year t1) (_year t2)
               <> compare (_month t1) (_month t2)
               <> compare (_day t1) (_day t2)
               <> compare (_hour t1) (_hour t2)
               <> compare (_minute t1) (_minute t2)

data Guard = Guard
  { _id         :: Int
  , _minsAsleep :: !Int
  , _minCount   :: !(IM.IntMap Int)
  } deriving (Show, Eq)

instance Ord Guard where
  compare g1 g2 = compare (_minsAsleep g1) (_minsAsleep g2)

mkGuard :: Int -> Guard
mkGuard id = Guard id 0 (IM.fromList [(i, 0) | i <- [0..59]])

updateGuard :: Int -- ^ Minute went to sleep
            -> Int -- ^ Minute woke up
            -> Guard
            -> Guard
updateGuard lastMin min guard = guard
  { _minsAsleep = _minsAsleep guard + sleepTime
  , _minCount   = foldl' incCount (_minCount guard) indexes
  }
 where
  sleepTime = min - lastMin
  indexes = [lastMin..min - 1]
  incCount counts min = IM.adjust (+ 1) min counts

mostAsleep :: Guard -> (Int, Int)
mostAsleep = maximumBy (\a b -> compare (snd a) (snd b))
           . IM.toList
           . _minCount

mostAsleepMin :: Guard -> Int
mostAsleepMin = fst . mostAsleep

data Guards = Guards
  { _current       :: Int
  , _lastTimestamp :: Timestamp
  , _guards        :: IM.IntMap Guard
  } deriving (Show)

mkGuards :: Guards
mkGuards = Guards undefined undefined IM.empty

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

parseLine :: T.Text -> Maybe Timestamp
parseLine = eitherToMaybe . runParser parse ""
 where
  sc :: Parsec Void T.Text ()
  sc = L.space space1 empty empty

  parse = Timestamp
      <$> (char '[' *> L.decimal)
      <*> (char '-' *> L.decimal)
      <*> (char '-' *> L.decimal)
      <*> (sc *> L.decimal)
      <*> (char ':' *> L.decimal <* char ']')
      <*> (sc *> parseAction <* sc)

  parseAction = (FallsAsleep <$ try (string "falls asleep"))
            <|> (WakesUp <$ try (string "wakes up"))
            <|> (BeginShift <$> (string "Guard #" *> L.decimal))

-- | Assuming that the guard always is awake before the next BeginShift.
applyTimestamp :: Timestamp -> Guards -> Guards
applyTimestamp t guards = case _action t of
  BeginShift id -> guards'
    { _current = id
    , _guards  = if not $ IM.member id (_guards guards)
                   then IM.insert id (mkGuard id) (_guards guards)
                   else _guards guards
    }
  FallsAsleep -> guards'
  WakesUp     -> guards'
    { _guards = IM.adjust (updateGuard lastMin min)
                          (_current guards)
                          (_guards guards)
    }
 where
  min = _minute t
  lastMin = _minute $ _lastTimestamp guards
  guards' = guards { _lastTimestamp = t }

mostFrequentlyAsleepAt :: Guards -> (Guard, (Int, Int))
mostFrequentlyAsleepAt
  = maximumBy (\(_, (_, c1)) (_, (_, c2)) -> compare c1 c2)
  . fmap ((\guard -> (guard, mostAsleep guard)) . snd)
  . IM.toList
  . _guards

day4 :: FilePath -> IO ()
day4 path = do
  -- Part 1
  putStrLn "--- Part 1 ---"
  ls <- fmap T.pack . lines <$> readFile path
  let stamps    = sort . catMaybes . fmap parseLine $ ls
      guards    = foldl' (flip applyTimestamp) mkGuards stamps
      mostSleep = maximum . fmap snd . IM.toList . _guards $ guards
      bestMin   = mostAsleepMin mostSleep
  putStrLn $ "Guard: " ++ show mostSleep
  putStrLn $ "Min: " ++ show bestMin

  putStrLn ""

  -- Part 2
  putStrLn "--- Part 2 ---"
  let (g, (min, cnt)) = mostFrequentlyAsleepAt guards
  putStrLn $ "Guard: " ++ show g
          ++ " Min: " ++ show min
          ++ " Count: " ++ show cnt
