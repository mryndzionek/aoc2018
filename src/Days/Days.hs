module Days.Days (
    solutions,
    extras
) where

import Util
import Control.Exception.Assert
import Codec.Picture
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (nub, foldl')
import qualified Data.List.Safe as S
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

type Claim = (Int, Int, Int, Int, Int)
day3_ :: Str -> ([Claim], Map.Map (Int, Int) Integer, Int, Claim)
day3_ (Str input) = let claims = parse . clearout <$> lines input
                        clearout = map (\c -> if c `elem` "@:x" then ',' else c) . 
                            filter (`notElem` " #")
                        parse s = read $ "(" ++ s ++ ")"
                        overlappingMap :: Map.Map (Int, Int) Integer
                        overlappingMap = foldl' addArea Map.empty claims
                               where addArea m c = foldl' (\m' p' -> Map.insertWith (+) p' 1 m') m $ claimToCords c
                        overlappingArea = length $ Map.filter (>1) overlappingMap
                        claimToCords (_, x, y, w, h) = [(x', y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]
                        checkOverlap c = all (==1) $ map (fromMaybe 0 . (`Map.lookup` overlappingMap)) $ claimToCords c
                        noOverlap = let candidates = filter checkOverlap claims in
                            byPred assert "Only one candidate available" ((1 ==) . length) candidates
                                (head candidates)
                    in (claims, overlappingMap, overlappingArea, noOverlap)

day3Draw :: Str -> IO ()
day3Draw input = let (_, overlappingMap, _, noOverlap) = day3_ input
                     img = let (_, x', y', w, h) = noOverlap
                               pts = [(x'', y'') | x'' <- [x'..x'+w-1], y'' <- [y'..y'+h-1]]
                           in foldl' (\m p -> Map.insert p (-1) m) overlappingMap pts
                     palette :: Integer -> Maybe PixelRGB8
                     palette x = if x < 0 then Just $ PixelRGB8 255 0 0 else
                        cycle [PixelRGB8 r g b | let cols = [0, 64, 128, 255], r <- cols, g <- cols, b <- cols] S.!! x
                     pixelRenderer x y = fromMaybe (PixelRGB8 200 200 200)
                        (Map.lookup (x, y) img >>= (palette . (* 9) . fromIntegral))
                 in writePng "images/day3.png" $ generateImage pixelRenderer 1000 1000

day3 :: Str -> (Int, Int)
day3 input = let (_, _, overlappingArea, noOverlap') = day3_ input
                 noOverlap = let (id', _, _, _, _) = noOverlap' in id'
             in (overlappingArea, noOverlap)

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
    (  1, mkDay (day1, fileToStr "inputs/day1.txt", (454, Just 566) )),
    (  2, mkDay (day2, fileToStr "inputs/day2.txt", (8610, "iosnxmfkpabcjpdywvrtahluy") )),
    (  3, mkDay (day3, fileToStr "inputs/day3.txt", (118539, 1270) ))
    ]

extras :: IO ()
extras = fileToStr "inputs/day3.txt" >>= day3Draw