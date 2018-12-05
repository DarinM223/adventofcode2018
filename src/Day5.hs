module Day5 where

import Data.Char
import Data.Foldable (foldl')

react :: String -> String
react = reverse . foldl' pushStack []
 where
  canReact a b = ((isUpper a && isLower b) || (isLower a && isUpper b))
              && (toLower a == toLower b)
  pushStack [] ch = [ch]
  pushStack (top:rest) ch
    | canReact top ch = rest
    | otherwise       = ch:top:rest

allLetters :: String -> [Int]
allLetters s = fmap (length . react)
             . fmap (\l -> filter (\c -> toLower c /= l) s)
             $ ['a'..'z']

day5part1 :: FilePath -> IO ()
day5part1 path = do
  -- They decided to add a stupid newline to the file >:(
  s <- filter (/= '\n') <$> readFile path

  let result = react s
  putStrLn $ "Result string: " ++ result
  putStrLn $ "Original length: " ++ show (length s)
  putStrLn $ "New length: " ++ show (length result)

day5part2 :: FilePath -> IO ()
day5part2 path = do
  -- They decided to add a stupid newline to the file >:(
  s <- filter (/= '\n') <$> readFile path
  let letters = allLetters s
  print letters
  print (minimum letters)
