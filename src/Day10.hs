module Day10 where

import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char.Lexer as L

data Light = Light
  { _position :: !(Int, Int)
  , _velocity :: !(Int, Int)
  } deriving (Show, Eq)

updateLight :: Light -> Light
updateLight l@Light{ _position = (!x, !y), _velocity = (dx, dy) } =
  l { _position = (x + dx, y + dy) }

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

parseLine :: String -> Maybe Light
parseLine = eitherToMaybe . runParser parse ""
 where
  sc :: Parsec Void String ()
  sc = L.space space1 empty empty

  parseNum = L.signed sc L.decimal
  parse = Light <$> parseField "position" <*> (sc *> parseField "velocity")
  parseField field = (,)
    <$> (string (field ++ "=<") *> sc *> parseNum <* sc <* char ',')
    <*> (sc *> parseNum <* sc <* char '>')

fetchLights :: FilePath -> IO [Light]
fetchLights path = mapMaybe parseLine . lines <$> readFile path

messageBounds :: [Light] -> Maybe (Int, Int)
messageBounds l | maxy - miny <= 10 = Just (miny, maxy)
                | otherwise         = Nothing
 where
  positions = fmap _position l
  maxy = maximum $ snd <$> positions
  miny = minimum $ snd <$> positions

message :: (Int, Int) -> [Light] -> (Int, String)
message (miny, maxy) l = (maxX + 1, message)
 where
  message = do
    y <- [miny..maxy]
    x <- [0..maxX]
    if HM.member (x, y) map then return 'X' else return '.'
  maxX = maximum $ fmap (fst . _position) l
  map = HM.fromList $ (, True) . _position <$> l

printMessage :: Int -> String -> IO ()
printMessage amount s = case take amount s of
  [] -> return ()
  s' -> print s' >> printMessage amount (drop amount s)

run :: [Light] -> (Int, (Int, Int), [Light])
run = go 0
 where
  go !secs lights = case messageBounds lights of
    Just bounds -> (secs, bounds, lights)
    Nothing     -> go (secs + 1) (fmap updateLight lights)

day10 :: FilePath -> IO ()
day10 path = do
  lights <- fetchLights path
  let (secs, bounds, result) = run lights
      (xsize, msg) = message bounds result
  printMessage xsize msg
  putStrLn $ "Seconds: " ++ show secs
