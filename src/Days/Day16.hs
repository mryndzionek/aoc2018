{-# LANGUAGE ScopedTypeVariables #-}

module Days.Day16
  ( day16
  ) where

import           Prelude         hiding (head, init, (!!))

import           Data.Bits
import           Data.List.Safe
import           Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust, isJust)

import           Util

type Op = (Int, Int, Int, Int)

type Regs = [Int]

type Entry = (Regs, Op, Regs)

type State = M.Map Int Int

type Instr = State -> Int -> Int -> Int -> Maybe State

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

howManyMatch :: Entry -> Int
howManyMatch (bef, o, aft) =
  let bef' = M.fromList $ zip [0 ..] bef
      aft' = M.fromList $ zip [0 ..] aft
      (_, a, b, c) = o
   in length $
      filter (== True) $
      fmap ((== aft') . (\i -> fromJust $ i bef' a b c)) instrs

toQuad :: [Int] -> Maybe Op
toQuad as = do
  a <- as !! (0 :: Int)
  b <- as !! (1 :: Int)
  c <- as !! (2 :: Int)
  d <- as !! (3 :: Int)
  return (a, b, c, d)

parse :: [String] -> Maybe Entry
parse s = do
  b <- s !! (0 :: Int)
  i <- s !! (1 :: Int)
  a <- s !! (2 :: Int)
  b' <- read <$> splitOn "Before: " b !! (1 :: Int)
  i' <- toQuad $ read ("[" ++intercalate "," (splitOn " " i) ++ "]")
  a' <- read <$> splitOn "After: " a !! (1 :: Int)
  return (b', i', a')

getEntries :: [String] -> [Entry]
getEntries ls = fmap fromJust $ takeWhile isJust $ parse <$> chunksOf 4 ls

day16 :: Str -> Int
day16 (Str input) =
  let ls :: [String] = lines input
      es = getEntries ls
   in length $ filter (>= 3) (fmap howManyMatch es)
