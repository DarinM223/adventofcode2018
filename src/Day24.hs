module Day24 where

import Control.Monad.State
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.Ord (Down (Down), comparing)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type AttackType = String

data UnitType = Infection | Immune deriving (Show, Eq)

data Group = Group
  { _id         :: Int
  , _units      :: Int
  , _hp         :: Int
  , _power      :: Int
  , _initiative :: Int
  , _type       :: AttackType
  , _immunities :: [AttackType]
  , _weaknesses :: [AttackType]
  , _team       :: UnitType
  } deriving (Show, Eq)
instance Ord Group where
  compare g1 g2 = comparing effectivePower g1 g2
               <> comparing _initiative g1 g2

mapFromList :: [Group] -> IntMap Group
mapFromList gs = M.fromList [(_id g, g) | g <- gs]

parseData :: String -> (IntMap Group, IntMap Group)
parseData s = case evalState (runParserT parse "" s) 0 of
  Left e         -> error $ errorBundlePretty e
  Right (g1, g2) -> (mapFromList g1, mapFromList g2)
 where
  sc = L.space space1 empty empty

  parse :: ParsecT Void String (State Int) ([Group], [Group])
  parse = (,) <$> parseImmune <*> (newline *> parseInfection) <* eof
  parseImmune = string "Immune System:" *> newline *> many (parseGroup Immune)
  parseInfection = string "Infection:" *> newline *> many (parseGroup Infection)
  parseGroup team = do
    units <- L.decimal <* string " units each with "
    hp <- L.decimal <* string " hit points "
    (immunities, weaknesses) <- fromMaybe ([], []) <$> optional parseTypes
    power <- string "with an attack that does " *> L.decimal <* sc
    ty <- parseType <* string "damage at initiative "
    initiative <- L.decimal <* newline
    id <- get <* modify' (+ 1)
    return Group
      { _id         = id
      , _units      = units
      , _hp         = hp
      , _power      = power
      , _initiative = initiative
      , _type       = ty
      , _immunities = immunities
      , _weaknesses = weaknesses
      , _team       = team
      }
  parseTypes = char '(' *> parseTypes' <* char ')' <* sc
  parseTypes' = try (flip (,) <$> parseWeak <*> (string "; " *> parseNoEff))
            <|> try ((,) <$> parseNoEff <*> (string "; " *> parseWeak))
            <|> (flip (,) <$> parseWeak <*> pure [])
            <|> ((,) <$> parseNoEff <*> pure [])
  parseWeak = string "weak to " *> sepBy1 parseType (string ", ")
  parseNoEff = string "immune to " *> sepBy1 parseType (string ", ")
  parseType = L.lexeme sc (many lowerChar)

effectivePower :: Group -> Int
effectivePower g = _units g * _power g

data Attack = Attack
  { _sortBy    :: Int
  , _unitType  :: UnitType
  , _attacking :: Int
  , _defending :: Int
  } deriving (Show, Eq)
instance Ord Attack where
  compare = comparing _sortBy

newtype Target = Target { unTarget :: (Int, Group) }
  deriving (Show, Eq)

damage :: Target -> Int
damage = fst . unTarget

group :: Target -> Group
group = snd . unTarget

instance Ord Target where
  compare g1 g2 = comparing damage g1 g2
               <> comparing (effectivePower . group) g1 g2
               <> comparing (_initiative . group) g1 g2

takeDamage :: Int -> Group -> Group
takeDamage dmg g = g { _units = _units g - (dmg `div` _hp g) }

calcDamage :: AttackType -> Int -> Group -> Int
calcDamage ty pow g | ty `elem` _immunities g = 0
                    | ty `elem` _weaknesses g = pow * 2
                    | otherwise               = pow

select :: IntMap Group -> IntMap Group -> S.Set Attack
select selecting enemies =
  fst . foldl' chooseTarget (S.empty, enemies') . desc $ selecting
 where
  desc = sortOn Down . M.elems
  enemies' = S.fromList $ M.elems enemies
  chooseTarget (!attacks, !other) g = case (selected, attack) of
    (Just t, Just a) -> (S.insert a attacks, S.delete (group t) other)
    _                -> (attacks, other)
   where
    mapTarget t = Target (calcDamage (_type g) (effectivePower g) t, t)
    targets = S.map mapTarget other
    selected = S.lookupMax targets
    attack = selected >>= \(Target (dmg, s)) ->
      if dmg == 0
        then Nothing
        else Just (Attack (_initiative g) (_team g) (_id g) (_id s))

turn :: IntMap Group -> IntMap Group -> (IntMap Group, IntMap Group)
turn infection immune =
  foldl' go (infection, immune) . S.toDescList $ attacks
 where
  attacks = S.union (select infection immune) (select immune infection)
  go (!infection, !immune) (Attack _ ty i1 i2) = case ty of
    Infection -> (infection, fromMaybe immune (group' immune))
    Immune    -> (fromMaybe infection (group' infection), immune)
   where
    g1 = case ty of
      Infection -> M.lookup i1 infection
      Immune    -> M.lookup i1 immune
    g2 = case ty of
      Infection -> M.lookup i2 immune
      Immune    -> M.lookup i2 infection
    dmg g1 = calcDamage (_type g1) (effectivePower g1)
    group' team = updateGroup <$> (dmg <$> g1 <*> g2) <*> g2 <*> pure team
  updateGroup dmg g team
    | _units g' <= 0 = M.delete (_id g) team
    | otherwise      = M.insert (_id g) g' team
   where g' = takeDamage dmg g

simulation :: IntMap Group -> IntMap Group -> (Bool, Int)
simulation !infection !immune
  | M.null infection = (True, countUnits immune)
  | M.null immune    = (False, countUnits infection)
  | stalemate        = (False, countUnits infection)
  | otherwise        = simulation infection' immune'
 where
  (infection', immune') = turn infection immune
  countUnits = foldl' (\acc g -> acc + _units g) 0
  stalemate = immune == immune' && infection == infection'

day24part1 :: FilePath -> IO ()
day24part1 path = do
  (immune, infection) <- parseData <$> readFile path
  let (_, units) = simulation infection immune
  print units

checkIndex :: IntMap Group -> IntMap Group -> Int -> (Bool, Int)
checkIndex inf imm i = simulation inf imm'
 where imm' = M.map (\g -> g { _power = _power g + i }) imm

findWin :: IntMap Group -> IntMap Group -> Int
findWin = go 100
 where
  go i !inf !imm = if fst (checkIndex inf imm i) then i else go (i * 2) inf imm

binarySearch :: IntMap Group -> IntMap Group -> Int -> Int -> Maybe Int
binarySearch inf imm start end
  | end < start = Nothing
  | won         = binarySearch inf imm start (mid - 1) <|> Just mid
  | otherwise   = binarySearch inf imm (mid + 1) end
 where
  mid = (start + end) `div` 2
  (won, _) = checkIndex inf imm mid

day24part2 :: FilePath -> IO ()
day24part2 path = do
  (immune, infection) <- parseData <$> readFile path
  let winIndex = findWin infection immune
      smaller  = binarySearch infection immune 0 winIndex
      result   = snd . checkIndex infection immune <$> smaller
  print result
