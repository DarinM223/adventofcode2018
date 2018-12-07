module Day7 where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe (catMaybes)
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

mkGraphNode :: Char -> IO GraphNode'
mkGraphNode c = newIORef $ GraphNode c [] 0

type Graph = [GraphNode']

buildGraph :: [(Char, Char)] -> IO Graph
buildGraph = fmap (fmap snd . IM.toList) . foldlM buildEdges IM.empty
 where
  buildEdges :: IM.IntMap GraphNode' -> (Char, Char) -> IO (IM.IntMap GraphNode')
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

buildInstructions :: Graph -> IO String
buildInstructions graph = findAvailable graph >>= \case
  (t@(_, available):_) ->
    removeNode t graph >>= fmap (_name available:) . buildInstructions
  _ -> return []

day7part1 :: FilePath -> IO ()
day7part1 path = do
  ls <- catMaybes . fmap parseLine . lines <$> readFile path
  graph <- buildGraph ls
  instructions <- buildInstructions graph
  print instructions
