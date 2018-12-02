module Day1 where

import Conduit
import Data.Char (isDigit, ord)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.IntSet as IS
import qualified Data.Text as T

parseLine :: Text -> Maybe Int
parseLine t = case T.head t of
  '+' -> num
  '-' -> negate <$> num
  _   -> Nothing
 where
  num = parseNumber 0 (T.tail t)

  chToInt :: Char -> Maybe Int
  chToInt c
    | isDigit c = Just $ ord c - ord '0'
    | otherwise = Nothing

  parseNumber :: Int -> Text -> Maybe Int
  parseNumber !acc t
    | T.null t  = Just acc
    | otherwise = ((+ acc * 10) <$> chToInt (T.head t))
              >>= flip parseNumber (T.tail t)

day1part1 :: FilePath -> IO Int
day1part1 path = runConduitRes
  $  CB.sourceFile path
  .| CT.decode CT.utf8
  .| CT.lines
  .| CL.fold (\acc line -> acc + fromMaybe 0 (parseLine line)) 0

type State = (Int, IS.IntSet)

day1part2 :: FilePath -> IO (Maybe Int)
day1part2 path = runConduitRes $ streamFile path (0, IS.fromList [0])

findRepeat :: (MonadResource m, MonadThrow m)
           => FilePath
           -> State
           -> ConduitM Text Void m (Maybe Int)
findRepeat path (!acc, !set) = await >>= \case
  Just line -> do
    let acc' = acc + fromJust (parseLine line)
    if IS.member acc' set
      then Just acc' <$ CL.sinkNull
      else findRepeat path (acc', IS.insert acc' set)
  Nothing -> streamFile path (acc, set)

streamFile :: (MonadResource m, MonadThrow m)
           => FilePath
           -> State
           -> ConduitM i Void m (Maybe Int)
streamFile path state
  =  CB.sourceFile path
  .| CT.decode CT.utf8
  .| CT.lines
  .| findRepeat path state
