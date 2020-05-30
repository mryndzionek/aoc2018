{-# LANGUAGE ScopedTypeVariables #-}

module Days.Day16
  ( day16
  ) where

import           Prelude              hiding (head, init, tail, (!!))

import           Data.Bits
import           Data.List.Safe
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import qualified Data.Set             as S

import           Text.Megaparsec      hiding (State, count, parse)
import           Text.Megaparsec.Char

import           Control.Monad.State

import           Util

data Inst =
  Inst Int
       Int
       Int
       Int
  deriving (Show)

type Regs = [Int]

data Sample =
  Sample Regs
         Inst
         Regs
  deriving (Show)

type St = M.Map Int Int

type Instr = St -> Int -> Int -> Int -> Maybe St

addr :: Instr
addr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $ M.insert c (va + vb) s

addi :: Instr
addi s a b c = do
  va <- M.lookup a s
  return $ M.insert c (va + b) s

mulr :: Instr
mulr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $ M.insert c (va * vb) s

muli :: Instr
muli s a b c = do
  va <- M.lookup a s
  return $ M.insert c (va * b) s

banr :: Instr
banr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $ M.insert c (va .&. vb) s

bani :: Instr
bani s a b c = do
  va <- M.lookup a s
  return $ M.insert c (va .&. b) s

borr :: Instr
borr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $ M.insert c (va .|. vb) s

bori :: Instr
bori s a b c = do
  va <- M.lookup a s
  return $ M.insert c (va .|. b) s

setr :: Instr
setr s a _ c = do
  va <- M.lookup a s
  return $ M.insert c va s

seti :: Instr
seti s a _ c = return $ M.insert c a s

gtir :: Instr
gtir s a b c = do
  vb <- M.lookup b s
  return $
    M.insert
      c
      (if a > vb
         then 1
         else 0)
      s

gtri :: Instr
gtri s a b c = do
  va <- M.lookup a s
  return $
    M.insert
      c
      (if va > b
         then 1
         else 0)
      s

gtrr :: Instr
gtrr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $
    M.insert
      c
      (if va > vb
         then 1
         else 0)
      s

eqir :: Instr
eqir s a b c = do
  vb <- M.lookup b s
  return $
    M.insert
      c
      (if a == vb
         then 1
         else 0)
      s

eqri :: Instr
eqri s a b c = do
  va <- M.lookup a s
  return $
    M.insert
      c
      (if va == b
         then 1
         else 0)
      s

eqrr :: Instr
eqrr s a b c = do
  va <- M.lookup a s
  vb <- M.lookup b s
  return $
    M.insert
      c
      (if va == vb
         then 1
         else 0)
      s

instrs :: [Instr]
instrs =
  [ addr
  , addi
  , mulr
  , muli
  , banr
  , bani
  , borr
  , bori
  , setr
  , seti
  , gtir
  , gtri
  , gtrr
  , eqir
  , eqri
  , eqrr
  ]

whichMatch :: Sample -> [Int]
whichMatch (Sample bef o aft) =
  let bef' = M.fromList $ zip [0 ..] bef
      aft' = M.fromList $ zip [0 ..] aft
      Inst _ a b c = o
      mat = fmap ((== aft') . (\i -> fromJust $ i bef' a b c)) instrs
   in fmap fst $ filter ((== True) . snd) $ zip [0 ..] mat

lut :: (Integral n, Traversable t) => t [n] -> [Instr]
lut cs = concatMap (instrs !!) sat
  where
    sat = fst ((fromJust . head) (accum pick cs S.empty))
    pick pos s = [(p, S.insert p s) | p <- pos, p `S.notMember` s]
    accum f = runStateT . traverse (StateT . f)

getConstraints :: [Sample] -> [[Int]]
getConstraints es =
  fmap S.toList $
  M.elems $
  foldl' go (M.fromList $ zip [0 .. length instrs - 1] (repeat S.empty)) es
  where
    go m e@(Sample _ (Inst o _ _ _) _) =
      M.adjust (\a -> S.union a (S.fromList (whichMatch e))) o m

parseRegs :: Parser Regs
parseRegs = between "[" "]" (number `sepBy` ", ")

parseSample :: Parser Sample
parseSample =
  Sample <$> between "Before: " newline parseRegs <*> parseInst <*>
  between "After:  " newline parseRegs <*
  newline

parseInst :: Parser Inst
parseInst =
  Inst <$> number <* space <*> number <* space <*> number <* space <*> number <*
  newline

parseInput :: Parser ([Sample], [Inst])
parseInput =
  (,) <$> many parseSample <* newline <* newline <*> many parseInst <* eof

parse :: String -> ([Sample], [Inst])
parse input =
  case runParser parseInput "" input of
    Left e  -> fail (errorBundlePretty e)
    Right a -> a

interpret :: [Instr] -> Inst -> State St ()
interpret l (Inst o a b c) = do
  s <- get
  let inst = fromJust $ l !! o
      s' = fromJust $ inst s a b c
  modify (const s')

day16 :: Str -> (Int, Int)
day16 (Str input) =
  let (samples, prog) = parse input
      part1 = length $ filter (>= 3) (fmap (length . whichMatch) samples)
      intpLut = lut $ getConstraints samples
      part2 =
        fromJust $
        M.lookup
          0
          (execState
             (traverse (interpret intpLut) prog)
             (M.fromList $ zip [0 .. 3] [0 ..]))
   in (part1, part2)
