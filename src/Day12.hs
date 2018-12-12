module Day12 where

import Data.Foldable
import qualified Data.HashMap.Strict as HM

parse :: FilePath -> IO (String, HM.HashMap String Char)
parse path = do
  (initLine:_:ruleLines) <- lines <$> readFile path
  let initStr = drop 15 initLine
      initGen = replicate 20 '.' ++ initStr ++ replicate 40 '.'
      map     = foldl' buildMap HM.empty ruleLines
  return (initGen, map)

day12part1 :: FilePath -> IO ()
day12part1 path = do
  (initGen, map) <- parse path
  let lastGen = foldl' (\gen _ -> generation map gen) initGen [1..20]
  putStrLn $ "Init gen: " ++ show initGen
  putStrLn lastGen
  putStrLn $ "Sum: " ++ show (sumGen lastGen)

day12part2 :: FilePath -> IO ()
day12part2 path = do
  (initGen, map) <- parse path
  let lastGen = foldl' (\gen _ -> generation map gen) initGen [1..50000000000]
  putStrLn lastGen
  putStrLn $ "Sum: " ++ show (sumGen lastGen)

buildMap :: HM.HashMap String Char -> String -> HM.HashMap String Char
buildMap map line = HM.insert key value map
 where
  key = take 5 line
  value = last line

sumGen :: String -> Int
sumGen = foldl' acc 0 . zip [-20..]
 where
  acc count (i, '#') = count + i
  acc count (_, _)   = count

generation :: HM.HashMap String Char -> String -> String
generation map s = (replicate 2 '.' ++ go map s) ++ replicate 2 '.'
 where
  go map (a:b:e:c:d:rest) = e':go map (b:e:c:d:rest)
   where
    e' = case HM.lookup (a:b:e:c:d:[]) map of
      Just plant -> plant
      Nothing    -> '.'
  go _ _ = []
