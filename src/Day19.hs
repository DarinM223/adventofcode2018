module Day19 where

import Data.Bits
import Data.Foldable
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
        <$> L.lexeme sc (many alphaNumChar)
        <*> (sc *> L.decimal)
        <*> (sc *> L.decimal)
        <*> (sc *> L.decimal)

runInstrs :: Int -> Instrs -> D.Registers -> D.Registers
runInstrs ipIdx !instrs !regs = case instrs V.!? (regs IM.! ipIdx) of
  Just (fnName, a, b, c) ->
    let regs' = updateRegs (strToFn fnName) a b c
    in runInstrs ipIdx instrs regs'
  Nothing -> regs
 where updateRegs fn a b c = IM.adjust (+ 1) ipIdx $ fn regs (0, a, b, c)

firstReg :: Int -> Int -> Instrs -> D.Registers -> IO ()
firstReg regIdx ipIdx !instrs !regs = case instrs V.!? (regs IM.! ipIdx) of
  Just (fnName, a, b, c) -> do
    let regs' = updateRegs (strToFn fnName) a b c
    print $ regs IM.! regIdx
    firstReg regIdx ipIdx instrs regs'
  Nothing -> return ()
 where updateRegs fn a b c = IM.adjust (+ 1) ipIdx $ fn regs (0, a, b, c)

day19part1 :: FilePath -> IO ()
day19part1 path = do
  (start, instrs) <- parseFile path
  let regs = runInstrs start instrs mkRegs
  print regs

-- Register is 5 and the number to get sum of factors of is 10551276
findFirstReg :: FilePath -> IO ()
findFirstReg path = do
  (start, instrs) <- parseFile path
  let regs = IM.insert 0 1 mkRegs
  firstReg 5 start instrs regs

sumOfFactors :: Int -> Int
sumOfFactors n = foldl' acc 0 [2..sqrt' n] + n + 1
 where
  sqrt' = floor . sqrt . fromIntegral
  acc sum i
    | n `rem` i == 0 = if i == revI then sum + i else sum + i + revI
    | otherwise      = sum
   where revI = n `div` i

day19part2 :: IO ()
day19part2 = print $ sumOfFactors 10551276
