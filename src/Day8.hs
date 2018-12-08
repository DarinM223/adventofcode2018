module Day8 where

import Data.Foldable
import Data.List.Split

data Node = Node
  { _numChildren :: Int
  , _numMeta     :: Int
  , _children    :: [Node]
  , _meta        :: [Int]
  } deriving (Show, Eq)

nodeSum :: Node -> Int
nodeSum n = foldl' (+) 0 (_meta n)
          + foldl' (\acc -> (+ acc) . nodeSum) 0 (_children n)

parseNode :: [Int] -> (Node, [Int])
parseNode (numChildren:numMeta:rest) =
  (Node numChildren numMeta children meta, nums'')
 where
  (children, nums') = parseMany parseNode numChildren rest
  (meta, nums'') = parseMany parseInt numMeta nums'
parseNode _ = error "Expected header"

parseInt :: [Int] -> (Int, [Int])
parseInt (e:es) = (e, es)
parseInt _      = error "Expected int"

parseMany :: ([Int] -> (a, [Int])) -> Int -> [Int] -> ([a], [Int])
parseMany f n nums = foldl' acc ([], nums) [1..n]
 where
  acc (!build, !nums) _ = let (e, nums') = f nums in (e:build, nums')

parseNums :: FilePath -> IO [Int]
parseNums path = fmap read . splitOn " " . filter (/= '\n') <$> readFile path

day8part1 :: FilePath -> IO ()
day8part1 path = do
  ns <- parseNums path
  let (node, _) = parseNode ns
  print $ nodeSum node

nodeValue :: Node -> Int
nodeValue n
  | _numChildren n == 0 = foldl' (+) 0 $ _meta n
  | otherwise           = foldl' acc 0 $ _meta n
 where
  len = length (_children n)
  acc sum i
    | i < 1 || i > len = sum
    | otherwise        = sum + nodeValue (_children n !! (len - i))

day8part2 :: FilePath -> IO ()
day8part2 path = do
  ns <- parseNums path
  let (node, _) = parseNode ns
  print $ nodeValue node
