module Days.Day7
  ( day7
  , day7Draw
  ) where

import Control.Lens hiding ((#))
import Data.Char
import qualified Data.List.Safe as S
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple


import Data.Colour.Palette.ColorSet
import Diagrams.Backend.Rasterific
import qualified Diagrams.Prelude as D
import Diagrams.Prelude ((#))

import Util

day7 :: Str -> (T.Text, Int)
day7 input =
  let (a, b) = day7_ input
   in (a, (^. _4) . head $ dropWhile (not . null . (^. _1)) b)

getPositions :: [[(Int, Char)]] -> Map.Map Char Int
getPositions states =
  let s = map (map snd) states
      f m a =
        let x = Set.difference (Set.fromList a) (Map.keysSet m)
            mx = maximum (Map.elems m)
        in Map.union m (Map.fromList $ zip (Set.toList x) [mx + 1 ..])
  in S.foldl f (Map.fromList $ zip (head s) [0 ..]) (tail s)

day7Draw :: Str -> IO ()
day7Draw input =
  let (_, b) = day7_ input
      states = map (^. _1) $ takeWhile (not . null . (^. _1)) b
      pos = getPositions states :: Map.Map Char Int
      row ((y0, h), s) = map (rect y0 h) s
      rect ::
           Int
        -> Int
        -> (Int, Char)
        -> (D.P2 Double, D.QDiagram B D.V2 Double D.Any)
      rect y h (_, l) =
        let x = 30 * fromMaybe 0 (Map.lookup l pos)
         in ( D.p2 (fromIntegral x, fromIntegral y + (fromIntegral h / 2))
            , D.rect (fromIntegral (30 :: Int)) (fromIntegral h) # D.lw D.none #
              D.fc (webColors $ ord l))
      example =
        let hs = map (fst . head) states
            ys = S.scanl' (+) 0 hs
            dy = zip ys  $ map (+ 1) hs
         in D.position (concatMap row $ zip dy states) # D.bg D.black
   in renderRasterific
        "images/day7_2.png"
        (D.mkHeight 1000)
        (example :: D.Diagram B)


day7_ :: Str -> (T.Text, [([(Int, Char)], String, String, Int)])
day7_ (Str input) =
  let steps =
        fromMaybe [] $
        mapM
          ((\a -> (,) <$> (a S.!! (1 :: Int)) <*> (a S.!! (7 :: Int))) .
           splitOn " ") $
        lines input :: [(String, String)]
      genMap :: [(String, String)] -> Map.Map Char String
      genMap s =
        Map.map S.sort $
        S.foldl' (\m (sa, sb) -> Map.insertWith (++) (head sa) sb m) Map.empty s
      succMap = genMap steps
      predMap = genMap (map swap steps)
      start =
        Set.toList $ Set.difference (Map.keysSet succMap) (Map.keysSet predMap)
      iter :: (T.Text, T.Text) -> (T.Text, T.Text)
      iter (visited, queued) =
        case Map.lookup (T.head queued) succMap of
          Nothing -> (nv, T.tail queued)
          Just v ->
            let new = filter (isVail (T.unpack nv)) v
             in (nv, T.pack $ S.sort (new ++ T.unpack (T.tail queued)))
        where
          nv = T.snoc visited (T.head queued)
      isVail v c =
        case Map.lookup c predMap of
          Nothing -> True
          Just p -> all (`elem` v) p
      order =
        fst . head . dropWhile (not . null . T.unpack . snd) $
        iterate iter ("", T.pack start)
      numWorkers = 5
      getTime c = ord c - ord 'A' + 61
      expand a = (getTime a, a)
      (work, idle, done) = (take numWorkers start, drop numWorkers start, [])
      process (w, i, d, t) =
        let (t', c) = head w
            d' = c : d
            i' =
              S.sort $
              i ++ filter (isVail d') (fromMaybe [] $ Map.lookup c succMap)
            free = numWorkers - length w + 1
            w' =
              S.sort $
              map expand (take free i') ++ map (\(a, b) -> (a - t', b)) (tail w)
         in (w', drop free i', d', t + t')
      parallelOrder = iterate process (map expand work, idle, done, 0)
   in (order, parallelOrder)
