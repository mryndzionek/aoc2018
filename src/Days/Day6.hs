module Days.Day6
  ( day6
  , day6Draw
  ) where

import Data.Function (on)
import qualified Data.List.Safe as S
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Util

import Codec.Picture
import Data.Colour.Palette.ColorSet
import Data.Colour.SRGB

points :: Str -> [(Int, Int)]
points (Str input) =
  (\(a:(b:_)) -> (a, b)) . map read . splitOn "," <$> lines input

genCords :: (Int, Int) -> (Int, Int) -> Int -> Set.Set (Int, Int)
genCords (x1, y1) (x2, y2) d' =
  Set.fromList [(x, y) | y <- [y1 - d' .. y2 + d'], x <- [x1 - d' .. x2 + d']]

getCordMap ::
     [(Int, Int)] -> Set.Set (Int, Int) -> Map.Map (Int, Int) (Int, Int)
getCordMap pts c =
  let findMin ps =
        let pss = S.sortBy (compare `on` snd) ps
            (p, h') = head pss
            cands = takeWhile ((== h') . snd) pss
         in if length cands > 1
              then Nothing
              else Just p
      cmin c' = (findMin . fmap (\p -> (p, manhattan c' p))) pts
      f m c'' =
        case cmin c'' of
          Nothing -> m
          Just p -> Map.insert c'' p m
   in S.foldl' f Map.empty c

day6 :: Str -> (Int, Int)
day6 input =
  let pts = points input
      tl = (minimum $ map fst pts, minimum $ map snd pts)
      br = (maximum $ map fst pts, maximum $ map snd pts)
      c1 = genCords tl br 0
      c2 = Set.difference (genCords tl br 1) c1
      m1 = getCordMap pts c1
      m2 = Map.union m1 $ getCordMap pts c2
      area m =
        Set.fromList . S.sortBy (flip compare) . map snd $ count (Map.elems m)
      maxAarea1 = maximum $ (Set.intersection `on` area) m1 m2
      maxAarea2 =
        length $ Set.filter (\p -> (< 10000) . sum $ fmap (manhattan p) pts) c1
   in (maxAarea1, maxAarea2)

day6Draw :: Str -> IO ()
day6Draw input =
  let pts = points input
      c = genCords (0, 0) (600, 600) 0
      m1 = getCordMap pts c
      m2 = Set.foldl' crit Map.empty c
        where
          crit m a =
            let s = sum $ fmap (manhattan a) pts
             in if abs (s - 10000) < 100
                  then m
                  else Map.insert a (s `rem` 255) m
      markPoints s m = let (_, m') = Map.partitionWithKey (\k _ -> any (<s) $ map (manhattan k) pts) m in m'
      colToPixel col =
        let rgb = toSRGB24 col
         in Just $
            PixelRGB8 (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
      palette1 (x, y) = colToPixel $ infiniteWebColors !! (x + y)
      palette2 a = colToPixel $ rybColor a
      pixelRenderer p m x y =
        fromMaybe (PixelRGB8 0 0 0) (Map.lookup (x, y) m >>= p)
   in do writePng "images/day6_1.png" $
           generateImage (pixelRenderer palette1 (markPoints 2 m1)) 600 600
         writePng "images/day6_2.png" $
           generateImage (pixelRenderer palette2 (markPoints 4 m2)) 600 600
