{-# LANGUAGE BangPatterns #-}

module Days.Day11
  ( day11
  ) where

import Control.Parallel.Strategies
import Data.Function (on)
import qualified Data.List.Safe as S
import Data.Matrix
import qualified Data.Vector as V

size :: Int
size = 300

hDigit :: Integral a => a -> a
hDigit n = (n `div` 100) `mod` 10

powerLevel :: Integral a => a -> (a, a) -> a
powerLevel sn (x, y) =
  let rackId = x + 10
      power = ((rackId * y) + sn) * rackId
   in hDigit power - 5

powerGrid :: Int -> Matrix Int
powerGrid sn = matrix size size (powerLevel sn)

calc :: Matrix Int -> Int -> Matrix Int
calc pg d =
  let d' = size - d + 1
      gen (x, y) =
        sum (V.take d $ V.drop (y - 1) $ getRow (x + d - 1) pg) +
        sum (V.take (d - 1) $ V.drop (x - 1) $ getCol (y + d - 1) pg)
   in matrix d' d' gen

toIndexed :: Matrix a -> Matrix (a, (Int, Int))
toIndexed m = matrix (nrows m) (ncols m) (\e@(x, y) -> (m ! e, (x, y)))

powerLevels :: Matrix Int -> Int -> [Matrix Int]
powerLevels pg n =
  S.scanl'
    (\b a -> submatrix 1 (nrows a) 1 (ncols a) b + a)
    pg
    ((parMap rdeepseq) (calc pg) [2 .. n])

day11 :: Int -> ((Int, Int), (Int, (Int, Int)))
day11 sn =
  let pg = powerGrid sn
      pgs = powerLevels pg size
      maxp = fmap (maximum . toIndexed) pgs
      cord = snd $ maxp !! 2
      pid = head $ S.sortBy (flip compare `on` snd) $ zip [1 ..] maxp
   in (cord, (fst pid, snd $ snd pid))
