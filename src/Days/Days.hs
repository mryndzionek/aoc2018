module Days.Days (
    solutions,
) where

import Util
import Control.Exception.Assert
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

day1 :: Str -> (Integer, Integer)
day1 (Str input) = let freq = map read $ lines $ filter (/= '+') input
                       freqSt (_, f', fs) f = let nf = (f + f') in (Set.member nf fs, nf, Set.insert nf fs)
                       freqSets = map (\(a, b, _) -> (a, b)) $ scanl freqSt (False, 0, Set.empty) (cycle freq)
                       fstTwice = snd . head $ dropWhile (not . fst) freqSets
                   in (sum freq, fstTwice)

day2 :: Str -> (Int, String)
day2 (Str input) = let ids = lines input
                       counts c = map fst . (count :: [Int] -> [(Int, Integer)]) . map snd $ count c
                       find' n = filter ((n `elem`) . counts) ids
                       checksum = let (twos, threes) = (find' 2, find' 3) in length twos * length threes
                       isCandidate i1 i2 = (1 ==) . length . filter id $ zipWith (/=) i1 i2
                       candidates = [(a, b) | a <- ids, b <- ids, a /= b, isCandidate a b]
                       correct = byPred assert "Only one candidate available" ((1 ==) . length) candidates
                                        (map fst . filter (uncurry (==)) . uncurry zip) $ head candidates
                   in  (checksum, correct)

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
    (  1, mkDay (day1, fileToStr "inputs/day1.txt", (454, 566) )),
    (  2, mkDay (day2, fileToStr "inputs/day2.txt", (8610, "iosnxmfkpabcjpdywvrtahluy") ))
    ]
