module Day2 where

import Conduit
import Control.Monad (guard)
import Data.Char (ord)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

buildMap :: T.Text -> IM.IntMap Int
buildMap = T.foldl' accum IM.empty
 where
  accum map c
    | IM.member (ord c) map = IM.adjust (+ 1) (ord c) map
    | otherwise             = IM.insert (ord c) 1 map

count :: (Int, Int) -> T.Text -> (Int, Int)
count (!numTwos, !numThrees) t = (numTwos', numThrees')
 where
  map = buildMap t
  hasExact n = (> 0) . IM.size . IM.filter (== n)
  numTwos' = if hasExact 2 map then numTwos + 1 else numTwos
  numThrees' = if hasExact 3 map then numThrees + 1 else numThrees

day2part1 :: FilePath -> IO (Int, Int)
day2part1 path = runConduitRes
  $  CB.sourceFile path
  .| CT.decode CT.utf8
  .| CT.lines
  .| CL.fold count (0, 0)

day2part2 :: FilePath -> IO ()
day2part2 path = do
  ls <- fmap T.pack . lines <$> readFile path
  let matches = findMatches ls
  print matches
  print $ fmap (\(a, b) -> takeOutDiff a b) matches

findMatches :: [T.Text] -> [(T.Text, T.Text)]
findMatches ts = do
  t1 <- ts
  t2 <- filter (/= t1) ts
  guard (diffByOne t1 t2)
  return (t1, t2)

takeOutDiff :: T.Text -> T.Text -> T.Text
takeOutDiff t1 t2
  | T.null t1 || T.null t2 = T.empty
  | T.head t1 == T.head t2 = T.cons (T.head t1) $ takeOutDiff (T.tail t1) (T.tail t2)
  | otherwise              = takeOutDiff (T.tail t1) (T.tail t2)

diffByOne :: T.Text -> T.Text -> Bool
diffByOne t1 t2 = go t1 t2 False
 where
  go t1 t2 diff
    | T.length t1 /= T.length t2 = False
    | T.null t1 && T.null t2     = diff
    | T.head t1 == T.head t2     = go (T.tail t1) (T.tail t2) diff
    | otherwise                  = if diff
                                     then False
                                     else go (T.tail t1) (T.tail t2) True
