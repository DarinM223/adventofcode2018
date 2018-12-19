module Day19 where

import Control.Monad
import Data.Bits
import Data.Vector (Vector)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Day16 as D
import qualified Text.Megaparsec.Char.Lexer as L

type InstrFn = (D.Registers -> D.Instruction -> D.Registers)
type Instrs = Vector (String, Int, Int, Int)

strToFn :: String -> InstrFn
strToFn "addr" = D.opr (+)
strToFn "addi" = D.opi (+)
strToFn "mulr" = D.opr (*)
strToFn "muli" = D.opi (*)
strToFn "banr" = D.opr (.&.)
strToFn "bani" = D.opi (.&.)
strToFn "borr" = D.opr (.|.)
strToFn "bori" = D.opi (.|.)
strToFn "setr" = D.setr
strToFn "seti" = D.seti
strToFn "gtir" = D.gtir
strToFn "gtri" = D.opi D.gt
strToFn "gtrr" = D.opr D.gt
strToFn "eqir" = D.eqir
strToFn "eqri" = D.opi D.eq
strToFn "eqrr" = D.opr D.eq
strToFn _      = error "Invalid op"

mkRegs :: D.Registers
mkRegs = IM.fromList $ zip [0..] $ replicate 6 0

parseFile :: FilePath -> IO (Int, Vector (String, Int, Int, Int))
parseFile path = do
  (instrLine:ls) <- lines <$> readFile path
  let starting = read (drop 4 instrLine) :: Int
      instrs   = V.fromList $ fmap parseLine ls
  return (starting, instrs)
 where
  parseLine s = case runParser parse "" s of
    Left e  -> error $ errorBundlePretty e
    Right v -> v
   where
    sc :: Parsec Void String ()
    sc = L.space space1 empty empty

    parse = (,,,)
        <$> (L.lexeme sc (many alphaNumChar))
        <*> (sc *> L.decimal)
        <*> (sc *> L.decimal)
        <*> (sc *> L.decimal)

-- I am an idiot who forgot that the index of the instruction pointer
-- doesn't have to be at 0 >_<
runInstrs :: Int -> Instrs -> D.Registers -> D.Registers
runInstrs ipIdx !instrs !regs = case instrs V.!? (regs IM.! ipIdx) of
  Just (fnName, a, b, c) ->
    let regs' = updateRegs (strToFn fnName) a b c
    in runInstrs ipIdx instrs regs'
  Nothing -> regs
 where
  updateRegs fn a b c = IM.adjust (+ 1) ipIdx $ fn regs (0, a, b, c)

day19part1 :: FilePath -> IO ()
day19part1 path = do
  (start, instrs) <- parseFile path
  print start
  print $ length instrs
  let regs = runInstrs start instrs mkRegs
  print regs

day19main :: IO ()
day19main = day19part1 "resources/day19/input"
