module Day7 where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe (catMaybes, isNothing)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap as IM

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

parseLine :: String -> Maybe (Char, Char)
parseLine = eitherToMaybe . runParser parse ""
 where
  parse :: Parsec Void String (Char, Char)
  parse = (,)
    <$> (string "Step " *> upperChar)
    <*> (string " must be finished before step " *> upperChar)

type GraphNode' = IORef GraphNode
data GraphNode = GraphNode
  { _name     :: Char
  , _edges    :: [GraphNode']
  , _incoming :: Int
  }
instance Show GraphNode where
  show = show . _name
instance Eq GraphNode where
  a == b = _name a == _name b
instance Ord GraphNode where
  compare a b = compare (_name a) (_name b)
instance Show (IORef GraphNode) where
  show _ = ""

mkGraphNode :: Char -> IO GraphNode'
mkGraphNode c = newIORef $ GraphNode c [] 0

type Graph = [GraphNode']

buildGraph :: [(Char, Char)] -> IO Graph
buildGraph = fmap (fmap snd . IM.toList) . foldlM buildEdges IM.empty
 where
  buildEdges map (source, sink)
    | not $ IM.member source' map = do
      map' <- IM.insert source' <$> mkGraphNode source <*> pure map
      buildEdges map' (source, sink)
    | not $ IM.member sink' map = do
      map' <- IM.insert sink' <$> mkGraphNode sink <*> pure map
      buildEdges map' (source, sink)
    | otherwise = case (IM.lookup source' map, IM.lookup sink' map) of
      (Just sourceRef, Just sinkRef) -> map <$ do
        modifyIORef' sourceRef $ \sourceData -> sourceData
          { _edges = sinkRef:_edges sourceData }
        modifyIORef' sinkRef $ \sinkData -> sinkData
          { _incoming = _incoming sinkData + 1 }
      _ -> error "Source or sink doesn't exist"
   where
    source' = ord source
    sink'   = ord sink

findAvailable :: Graph -> IO [(GraphNode', GraphNode)]
findAvailable graph = do
  available <- filterM (fmap ((== 0) . _incoming) . readIORef) graph
  zipped <- zip available <$> mapM readIORef available
  return $ sortBy (\(_, e1) (_, e2) -> compare e1 e2) zipped

removeNode :: (GraphNode', GraphNode) -> Graph -> IO Graph
removeNode (node', node) graph = do
  forM_ (_edges node) $ \edgeRef ->
    modifyIORef' edgeRef $ \edge -> edge { _incoming = _incoming edge - 1 }
  return $ filter (/= node') graph

data Elf = Elf
  { _work :: Maybe (GraphNode', GraphNode)
  , _time :: Int
  } deriving (Show, Eq)

buildInstructions :: Graph -> IO String
buildInstructions graph = findAvailable graph >>= \case
  (t@(_, available):_) ->
    removeNode t graph >>= fmap (_name available:) . buildInstructions
  _ -> return []

testTimeFn :: Char -> Int
testTimeFn c = ord c - ord 'A' + 1

realTimeFn :: Char -> Int
realTimeFn c = ord c - ord 'A' + 61

multElvesTime :: (Char -> Int) -> [Elf] -> Graph -> IO Int
multElvesTime timeFn = go 0
 where
  -- What a complete mess :(

  elfTime (Elf m t)  = maybe 1000 (const t) m
  workTime = timeFn . _name . snd

  addWork _ []                      = []
  addWork [] rest                   = rest
  addWork (w:ws) (Elf Nothing _:es) = Elf (Just w) (workTime w):addWork ws es
  addWork ws (Elf (Just w) t:es)    = Elf (Just w) t:addWork ws es

  updateElves minTime = foldr updateElf ([], [])
   where
    updateElf (Elf Nothing _) (elves, completed) =
      (Elf Nothing 0:elves, completed)
    updateElf (Elf (Just w) time) (elves, completed)
      | timeDiff == 0 = (Elf Nothing 0:elves, w:completed)
      | otherwise     = (Elf (Just w) timeDiff:elves, completed)
     where timeDiff = if time - minTime < 0 then 0 else time - minTime

  filterNames names = filter (\(_, e) -> not $ (_name e) `elem` names)

  go time elves graph = findAvailable graph >>= \case
    []        -> return $ time + maximum (fmap _time elves)
    available -> do
      let names          = fmap (_name . snd) . catMaybes . fmap _work $ elves
          availableElves = length $ filter (isNothing . _work) elves
          assigned       = take availableElves $ filterNames names available
          elves'         = addWork assigned elves
          minTime        = minimum $ fmap elfTime elves'
      let (elves'', completed) = updateElves minTime elves'
      graph' <- foldlM (\graph t -> removeNode t graph) graph completed
      go (time + minTime) elves'' graph'

day7part1 :: FilePath -> IO ()
day7part1 path = do
  ls <- catMaybes . fmap parseLine . lines <$> readFile path
  graph <- buildGraph ls
  instructions <- buildInstructions graph
  print instructions

day7part2 :: (Char -> Int) -> FilePath -> IO ()
day7part2 f path = do
  ls <- catMaybes . fmap parseLine . lines <$> readFile path
  graph <- buildGraph ls
  time <- multElvesTime f (replicate 5 (Elf Nothing 0)) graph
  print time
