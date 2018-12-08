{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Days.Day8
  ( day8
  ) where

import Control.Lens
import Data.Function (on)
import qualified Data.List.Safe as S
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Util

data Node = Node
  { __id :: Int
  , _level :: Int
  , _metadata :: [Int]
  , __children :: [Int]
  } deriving (Show, Eq, Ord)

makeLenses ''Node

rec :: Int -> Int -> [Int] -> ([Node], Int, [Int])
rec i lev es
  | null es = ([], 0, [])
  | head es == 0 =
    let len = (head $ tail es)
        meta = take len (drop 2 es)
        rest = drop len (drop 2 es)
     in ([Node i lev meta []], i + 1, rest)
  | otherwise =
    let (n, i', r) = rec i (lev + 1) (drop 2 es)
        h = (head es - 1) : [head $ tail es]
        (n', i'', r') = rec i' lev (h ++ r)
        nodes = S.sortBy (compare `on` view level) (n ++ n')
        parent = head nodes
        childr = __id $ head (tail nodes)
        nodes' = tail nodes ++ [parent & _children %~ ([childr] ++)]
     in (nodes', i'', r')

day8 :: Str -> (Int, Int)
day8 (Str input) =
  let entries = (read <$> splitOn " " input)
      nodes = rec 0 0 entries ^. _1
      nodeMap = Map.fromList (map (\n -> (__id n, n)) nodes)
      licNum1 = sum $ concatMap (view metadata) nodes
      sumN n =
        if null (__children n)
          then sum (n ^. metadata)
          else let ids =
                     map ((S.!!) (__children n) . (\x -> x - 1)) (n ^. metadata)
                   nds = map (>>= f) ids
                   f i = do
                     n' <- Map.lookup i nodeMap
                     return $ sumN n'
                in sum $ map (fromMaybe 0) nds
   in (licNum1, sumN $ last nodes)
