module Days.Day12
  ( day12
  ) where

import Control.Lens
import Control.Parallel.Strategies
import Data.List.Safe
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Prelude hiding ((!!), head, tail)

import Util

parse :: String -> (Maybe String, Maybe (M.Map T.Text Char))
parse input =
  let ls = lines (filter (/= ' ') input)
      initState = splitOn ":" <$> head ls >>= (!! (1 :: Int))
      rules :: Maybe [(T.Text, Char)]
      rules =
        traverse
          ((\a -> (,) <$> (T.pack <$> head a) <*> ((a !! (1 :: Int)) >>= head)) .
           splitOn "=>")
          (drop 2 ls)
   in (initState, M.fromList <$> rules)

grow :: T.Text -> T.Text
grow s = T.concat ["....", s, "...."]

cut :: T.Text -> [T.Text]
cut = unfoldr f
  where
    f s'
      | T.length s' < 5 = Nothing
      | otherwise = Just (T.take 5 s', T.tail s')

replace :: M.Map T.Text Char -> T.Text -> T.Text
replace rs s =
  T.pack $ parMap rdeepseq (\r -> fromMaybe '.' $ M.lookup r rs) $ cut s

day12 :: Str -> (Maybe Int, Maybe Int)
day12 (Str input) =
  let (initState, rs) = parse input
      generations r i = iterate (grow . replace r) (grow $ T.pack i)
      sums l = map (countPlants l)
      diffs :: [Int] -> Maybe [Int]
      diffs as = zipWith (-) <$> tail as <*> pure as
      findCycle :: [Int] -> Maybe (Int, Int, Int)
      findCycle xs = zip3 [0 ..] xs <$> diffs xs >>= nWith 10 (==) (view _3)
      countPlants l a =
        sum $
        zipWith
          (\a' b ->
             if a' == '#'
               then b
               else 0)
          (T.unpack a)
          [-offset ..]
        where
          offset = (T.length a - l) `div` 2
   in fromMaybe (Nothing, Nothing) $ do
        g <- generations <$> rs <*> initState
        l <- length <$> initState
        let s = sums l g
        (n, a, c) <- findCycle s
        return (s !! (20 :: Int), Just $ c * (50000000000 - n) + a)
