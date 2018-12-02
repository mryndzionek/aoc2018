module Days.Days (
    solutions,
) where

import Util
import Control.Exception.Assert
import Data.Function (on)
import Data.List (nub)
import qualified Data.Map.Strict as Map

day1 :: Str -> (Integer, Maybe Integer)
day1 (Str input) = let freq = map read $ lines $ filter (/= '+') input
                       freqs = scanl1 (+) (cycle freq)
                   in (sum freq, firstRepeated freqs)

day2 :: Str -> (Int, String)
day2 (Str input) = let ids = lines input
                       counts :: String -> [Int]
                       counts c = nub . map snd $ count c
                       find' n = filter ((n `elem`) . counts) ids
                       checksum = ((*) `on` (length . find')) 2 3
                       isCandidate i1 i2 = (1 ==) . length . filter (uncurry (/=)) $ zip i1 i2
                       candidates = [(a, b) | a <- ids, b <- ids, a /= b, isCandidate a b]
                       correct = byPred assert "Only one candidate available" ((1 ==) . length) candidates
                                        (map fst . filter (uncurry (==)) . uncurry zip) $ head candidates
                   in  (checksum, correct)

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
    (  1, mkDay (day1, fileToStr "inputs/day1.txt", (454, Just 566) )),
    (  2, mkDay (day2, fileToStr "inputs/day2.txt", (8610, "iosnxmfkpabcjpdywvrtahluy") ))
    ]
