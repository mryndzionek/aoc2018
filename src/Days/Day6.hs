module Days.Day6 (day6, day6Draw) where

import Codec.Picture
import Data.List.Split
import Data.Function (on)
import qualified Data.List.Safe as S
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Util

points :: Str -> [(Int, Int)]
points (Str input) = ((\ (a : (b : _)) -> (a, b)) . map read . splitOn ",") <$> lines input

genCords :: (Int, Int) -> (Int, Int) -> Int -> Set.Set (Int, Int)
genCords (x1, y1) (x2, y2) d' = Set.fromList [(x, y) | y <- [y1-d'..y2+d'], x <- [x1-d'..x2+d']]

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

getCordMap :: [(Int, Int)] -> Set.Set (Int, Int) -> Map.Map (Int, Int) (Int, Int)
getCordMap pts c = let findMin ps = let pss = S.sortBy (compare `on` snd) ps
                                        (p, h') = head pss
                                        cands = takeWhile ((== h') . snd) pss
                                    in if length cands > 1 then Nothing else Just p
                       cmin c' = (findMin . fmap (\p -> (p, manhattan c' p))) pts
                       f m c'' = case cmin c'' of
                                Nothing -> m
                                Just p -> Map.insert c'' p m
                   in S.foldl' f Map.empty c

day6 :: Str -> (Int, Int)
day6 input = let pts = points input
                 tl = (minimum $ map fst pts, minimum $ map snd pts)
                 br = (maximum $ map fst pts, maximum $ map snd pts)
                 c1 = genCords tl br 0
                 c2 = Set.difference (genCords tl br 1) c1
                 m1 = getCordMap pts c1
                 m2 = Map.union m1 $ getCordMap pts c2
                 area m = Set.fromList . S.sortBy (flip compare) . map snd $ count (Map.elems m)
                 maxAarea1 = maximum $ (Set.intersection `on` area) m1 m2
                 maxAarea2 = length $ Set.filter (\p -> (<10000) . sum $ fmap (manhattan p) pts) c1
              in (maxAarea1, maxAarea2)

day6Draw :: Str -> IO ()
day6Draw input = let pts = points input
                     c = genCords (0, 0) (600, 600) 0
                     m1 = getCordMap pts c
                     m2 = Set.foldl' crit Map.empty c
                        where crit m a = let s = sum $ fmap (manhattan a) pts
                                         in if s < 10000 then m else Map.insert a (0, s `rem` 255) m
                     palette (x, y) = cycle [PixelRGB8 r g b | let cols = [0, 64, 128, 255],
                         r <- cols, g <- cols, b <- cols] S.!! (x + y)
                     pixelRenderer m x y = fromMaybe (PixelRGB8 0 0 0) (Map.lookup (x, y) m >>= palette)
                  in do
                    writePng "images/day6_1.png" $ generateImage (pixelRenderer m1) 600 600
                    writePng "images/day6_2.png" $ generateImage (pixelRenderer m2) 600 600