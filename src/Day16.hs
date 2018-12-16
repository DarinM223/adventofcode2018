module Day16 where

import Data.Bits
import Data.Foldable
import Data.Hashable
import Data.Maybe (catMaybes, fromJust)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char.Lexer as L

type Registers = IM.IntMap Int
type Instruction = (Int, Int, Int, Int)
newtype TaggedFn = TaggedFn
  { unTag :: (Int, (Registers -> Instruction -> Registers)) }
type MaybeOps = IM.IntMap [TaggedFn]

instance Show TaggedFn where
  show (TaggedFn (i, _)) = "Fn" ++ show i
instance Eq TaggedFn where
  (TaggedFn (i1, _)) == (TaggedFn (i2, _)) = i1 == i2
instance Hashable TaggedFn where
  hashWithSalt salt (TaggedFn (i, _)) = hashWithSalt salt i

mkRegs :: Registers
mkRegs = IM.fromList $ zip [0..] $ replicate 4 0

lookupFn :: Int -> MaybeOps -> (Registers -> Instruction -> Registers)
lookupFn i ops = snd $ unTag $ head $ ops IM.! i

opr :: (Int -> Int -> Int) -> Registers -> Instruction -> Registers
opr f regs (_, ra, rb, rc) = IM.insert rc (f (regs IM.! ra) (regs IM.! rb)) regs

opi :: (Int -> Int -> Int) -> Registers -> Instruction -> Registers
opi f regs (_, ra, b, rc) = IM.insert rc (f (regs IM.! ra) b) regs

opfns :: (Int -> Int -> Int) -> [Registers -> Instruction -> Registers]
opfns f = [opr f, opi f]

eq :: Int -> Int -> Int
eq a b = if a == b then 1 else 0

gt :: Int -> Int -> Int
gt a b = if a > b then 1 else 0

setr :: Registers -> Instruction -> Registers
setr regs (_, ra, _, rc) = IM.insert rc (regs IM.! ra) regs

seti :: Registers -> Instruction -> Registers
seti regs (_, a, _, rc) = IM.insert rc a regs

gtir :: Registers -> Instruction -> Registers
gtir regs (_, a, rb, rc) = IM.insert rc (gt a (regs IM.! rb)) regs

eqir :: Registers -> Instruction -> Registers
eqir regs (_, a, rb, rc) = IM.insert rc (eq a (regs IM.! rb)) regs

fns :: [Registers -> Instruction -> Registers]
fns = fns' ++ ops
 where
  ops = [(*), (+), (.&.), (.|.), gt, eq] >>= opfns
  fns' = [setr, seti, gtir, eqir]

taggedFns :: [TaggedFn]
taggedFns = TaggedFn <$> zip [0..] fns

numInstrs :: Registers -> Instruction -> Registers -> Int
numInstrs before instr after = length
                             . filter (== after)
                             . fmap (\fn -> fn before instr)
                             $ fns

instrs before instr = fmap (\fn -> fn before instr) fns

data Sample = Sample
  { _before :: Registers
  , _input  :: Instruction
  , _after  :: Registers
  } deriving (Show, Eq)

toRegisters :: [Int] -> Registers
toRegisters = IM.fromList . zip [0..]

countSamples :: [Sample] -> Int
countSamples = length . filter (>= 3) . fmap mapSample
 where
  mapSample (Sample b i a) = numInstrs b i a

parseFile :: FilePath -> IO ([Sample], [Instruction])
parseFile path = do
  s <- readFile path
  runParserT parse "" s >>= \case
    -- Didn't know that you can pretty print errors :(
    Left e  -> error $ errorBundlePretty e
    Right v -> return v
 where
  sc :: ParsecT Void String IO ()
  sc = L.space space1 empty empty

  toTuple (a:b:c:d:_) = (a, b, c, d)
  toTuple _           = error "Invalid tuple"

  -- endBy is like sepBy but ends with separator
  parse = (,)
      <$> many parseSample <* newline <* newline
      <*> endBy parseInstruction newline
      <*  eof

  parseRegisters = toRegisters
               <$> (char '[' *> sepBy L.decimal (string ", ") <* char ']')
  parseInstruction = toTuple <$> sepBy L.decimal (char ' ')
  parseSample = Sample
            <$> (string "Before:" *> sc *> parseRegisters <* newline)
            <*> parseInstruction <* newline
            <*> (string "After:" *> sc *> parseRegisters <* newline)
            <*  newline

reducePossibilities :: MaybeOps -> Sample -> MaybeOps
reducePossibilities ops (Sample before instr output) = case IM.lookup op ops of
  Just fns' -> IM.insert op (updateFns fns') ops
  Nothing   -> IM.insert op (updateFns taggedFns) ops
 where
  (op, _, _, _) = instr
  updateFns = filter (\fn -> (snd $ unTag fn) before instr == output)

reduceObvious :: MaybeOps -> MaybeOps
reduceObvious ops = go (fnSet ops) $ ops
 where
  fnSet = IS.fromList
        . fmap (fst . unTag . head . snd)
        . filter ((== 1) . length . snd)
        . IM.assocs
  go set ops
    | set == set' = ops'
    | otherwise   = go set' ops'
   where
    nonSingle = filter ((> 1) . length . snd) $ IM.assocs ops
    (set', ops') = foldl' build (set, ops) nonSingle
    build (!set, !ops) (op, fns)
      | length fns' == 1 = (IS.insert (fst $ unTag $ head fns') set, ops')
      | otherwise        = (set, ops')
     where
      fns' = filter (\(TaggedFn (i, _)) -> not $ IS.member i set) fns
      ops' = IM.insert op fns' ops

guessAndCheck :: MaybeOps -> Maybe MaybeOps
guessAndCheck ops = fmap
  (foldl' (\ops (fn, op) -> IM.insert op [fn] ops) ops)
  guesses
 where
  assocs = filter ((> 1) . length . snd) $ IM.assocs ops
  guesses = HM.toList <$> guessFns HM.empty assocs
  guessFns map [] = Just map
  guessFns map ((op, fns):rest) = case possibleOps of
    []    -> Nothing
    (r:_) -> Just r
   where
    fns' = filter (\fn -> not $ HM.member fn map) fns
    recur fn l = guessFns (HM.insert fn op map) rest:l
    possibleOps = catMaybes $ foldr recur [] fns'

execute :: MaybeOps -> Registers -> Instruction -> Registers
execute ops regs instr@(op, _, _, _) = (lookupFn op ops) regs instr

day16part1 :: FilePath -> IO ()
day16part1 path = do
  (samples, _) <- parseFile path
  print $ countSamples samples

day16part2 :: FilePath -> IO ()
day16part2 path = do
  (samples, instrs) <- parseFile path
  let ops   = foldl' reducePossibilities IM.empty samples
      ops'  = reduceObvious ops
      ops'' = fromJust $ guessAndCheck ops'
      regs  = foldl' (execute ops'') mkRegs instrs
  print ops
  print ops''
  print regs
