module Days.Days (
    solutions,
) where

import Util
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

day1 :: Str -> (Integer, Integer)
day1 (Str input) = let freq = map read $ lines $ filter (/= '+') input
                       freqSt (_, f', fs) f = let nf = (f + f') in (Set.member nf fs, nf, Set.insert nf fs)
                       freqSets = map (\(a, b, _) -> (a, b)) $ scanl freqSt (False, 0, Set.empty) (cycle freq)
                       fstTwice = snd . head $ dropWhile (not . fst) freqSets
                   in (sum freq, fstTwice)

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
    (  1, mkDay (day1, fileToStr "inputs/day1.txt", (454, 566) ))]
