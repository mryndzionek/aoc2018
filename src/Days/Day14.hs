module Days.Day14
  ( day14
  ) where

import Data.Char (digitToInt)
import Data.List (isPrefixOf, tails)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

digits :: Int -> [Int]
digits = map digitToInt . show

step :: (Int, Int, Seq Int) -> ([Int], (Int, Int, Seq Int))
step (e1, e2, rs) =
  let a = Seq.index rs e1
      b = Seq.index rs e2
      nd = digits (a + b)
      rs' = rs Seq.>< Seq.fromList nd
      wrap x = x `mod` length rs'
   in (nd, (wrap $ e1 + a + 1, wrap $ e2 + b + 1, rs'))

recipes :: [Int]
recipes =
  let gen s =
        let (d, s') = step s
         in d ++ gen s'
   in 3 : 7 : gen (0, 1, Seq.fromList [3, 7])

subLoc :: [Int] -> [Int] -> Int
subLoc xs = length . takeWhile (not . (xs `isPrefixOf`)) . tails

day14 :: Int -> (Integer, Int)
day14 n =
  let part1 = read . concatMap show . take 10 $ drop n recipes
      part2 = subLoc (digits n) recipes
   in (part1, part2)
