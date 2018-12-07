{-# LANGUAGE OverloadedStrings #-}

module Days.Day7
  ( day7
  ) where

import Control.Lens
import Data.Char
import qualified Data.List.Safe as S
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple

import Util

day7 :: Str -> (T.Text, Int)
day7 (Str input) =
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
      parallelOrder =
        (^. _4) . head . dropWhile (not . null . (^. _1)) $
        iterate process (map expand work, idle, done, 0)
   in (order, parallelOrder)
