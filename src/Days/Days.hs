{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Days.Days
  ( solutions
  , extras
  ) where

import Control.Exception.Assert
import Data.Char
import Data.Function (on)
import Data.List (nub)
import qualified Data.List.Safe as S
import qualified Data.Map.Strict as Map

import Days.Day3
import Days.Day4
import Days.Day6
import Days.Day7
import Days.Day8
import Days.Day9
import Days.Day10
import Days.Day11
import Days.Day12
import Days.Day13
import Days.Day14
import Days.Day15
import Days.Day16
import Util

day1 :: Str -> (Integer, Maybe Integer)
day1 (Str input) =
  let freq = map read $ lines $ filter (/= '+') input
      freqs = scanl1 (+) (cycle freq)
   in (sum freq, firstRepeated freqs)

day2 :: Str -> (Int, String)
day2 (Str input) =
  let ids = lines input
      counts :: String -> [Int]
      counts c = nub . map snd $ count c
      find' n = filter ((n `elem`) . counts) ids
      checksum = ((*) `on` (length . find')) 2 3
      isCandidate i1 i2 = (1 ==) . length . filter (uncurry (/=)) $ zip i1 i2
      candidates = [(a, b) | a <- ids, b <- ids, a /= b, isCandidate a b]
      correct =
        byPred
          assert
          "Only one candidate available"
          ((1 ==) . length)
          candidates
          (map fst . filter (uncurry (==)) . uncurry zip) $
        head candidates
   in (checksum, correct)

day5 :: Str -> (Int, Int)
day5 (Str input) =
  let polymer = head $ lines input
      react = length . S.foldl' reduce ""
      reduce b a
        | null b = [a]
        | trigger a (head b) = tail b
        | otherwise = a : b
      trigger a b = ((== 32) . abs) (ord a - ord b)
      polymers =
        fmap (($ polymer) . (\x -> filter (\y -> toLower y /= x))) ['a' .. 'z']
      minimal = minimum $ fmap react polymers
   in (react polymer, minimal)
solutions :: Map.Map Int (IO Solution)
solutions =
  Map.fromList
    [ (1, mkDay (day1, fileToStr "inputs/day1.txt", (454, Just 566)))
    , ( 2
      , mkDay
          ( day2
          , fileToStr "inputs/day2.txt"
          , (8610, "iosnxmfkpabcjpdywvrtahluy")))
    , (3, mkDay (day3, fileToStr "inputs/day3.txt", (118539, 1270)))
    , (4, mkDay (day4, fileToStr "inputs/day4.txt", (Just 67558, Just 78990)))
    , (5, mkDay (day5, fileToStr "inputs/day5.txt", (9526, 6694)))
    , (6, mkDay (day6, fileToStr "inputs/day6.txt", (3290, 45602)))
    , ( 7
      , mkDay
          ( day7
          , fileToStr "inputs/day7.txt"
          , ("MNOUBYITKXZFHQRJDASGCPEVWL", 893)))
    , (8, mkDay (day8, fileToStr "inputs/day8.txt", (43996, 35189)))
    , (9, mkDay (day9, pure (477, 70851, 100 * 70851), (374690, 3009951158)))
    , (10, mkDay (day10, fileToStr "inputs/day10.txt", ("EKALLKLB", Just 10227)))
    , (11, mkDay (day11, pure 5034, ((235, 63),(16, (229, 251)))))
    , (12, mkDay (day12, fileToStr "inputs/day12.txt", (Just 3230, Just 4400000000304)))
    , (13, mkDay (day13, fileToStr "inputs/day13.txt", ((116, 91), (8, 23))))
    , (14, mkDay (day14, pure 793061, (4138145721, 20276284)))
    , (15, mkDay (day15, fileToStr "inputs/day15.txt", (239010, 62468)))
    , (16, mkDay (day16, fileToStr "inputs/day16.txt", (529, 573)))
    ]

extras :: IO ()
extras = do
  fileToStr "inputs/day3.txt" >>= day3Draw
  fileToStr "inputs/day6.txt" >>= day6Draw
  fileToStr "inputs/day7.txt" >>= day7Draw
  fileToStr "inputs/day10.txt" >>= day10Draw
  fileToStr "inputs/day15.txt" >>= day15Draw
