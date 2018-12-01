module Days.Days (
    solutions,
) where

import Util
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

day1 :: Str -> (Integer, Integer)
day1 (Str input) = let freq = map read $ lines $ filter (/= '+') input :: [Integer]
                       freqs = cycle freq
                       freqSt (_, f', fs) f = let nf = (f + f') in (Set.member nf fs, nf, Set.insert nf fs)
                       freqSts = map (\(a, b, _) -> (a, b)) $ scanl freqSt (False, 0, Set.empty) freqs
                       fstTwice = snd . head $ dropWhile (not . fst) freqSts
                   in (sum freq, fstTwice)

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
    (  1, mkDay (day1, Str <$> readFile "inputs/day1.txt", (454, 566) ))]
