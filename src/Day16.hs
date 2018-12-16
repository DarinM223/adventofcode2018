module Day16 where

import Data.Bits
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap as IM
import qualified Text.Megaparsec.Char.Lexer as L

type Registers = IM.IntMap Int
type Instruction = (Int, Int, Int, Int)

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
  ops = [(*), (+), (-), (.&.), (.|.), gt, eq] >>= opfns
  fns' = [setr, seti, gtir, eqir]

numInstrs :: Registers -> Instruction -> Registers -> Int
numInstrs before instr after = length
                             . filter (== after)
                             . fmap (\fn -> fn before instr)
                             $ fns

instrs before instr = fmap (\fn -> fn before instr) fns

data Sample = Sample
  { _before :: [Int]
  , _input  :: Instruction
  , _after  :: [Int]
  } deriving (Show, Eq)

toRegisters :: [Int] -> Registers
toRegisters = IM.fromList . zip [0..]

countSamples :: [Sample] -> Int
countSamples = length . filter (>= 3) . fmap mapSample
 where
  mapSample (Sample b i a) = numInstrs (toRegisters b) i (toRegisters a)

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

  parseRegisters = char '[' *> sepBy L.decimal (string ", ") <* char ']'
  parseInstruction = toTuple <$> sepBy L.decimal (char ' ')
  parseSample = Sample
            <$> (string "Before:" *> sc *> parseRegisters <* newline)
            <*> parseInstruction <* newline
            <*> (string "After:" *> sc *> parseRegisters <* newline)
            <*  newline

day16part1 :: FilePath -> IO ()
day16part1 path = do
  (samples, _) <- parseFile path
  print $ countSamples samples
