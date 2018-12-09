{-# LANGUAGE GADTs #-}

module Days.Day9
  ( day9
  ) where

import Control.Lens
import Data.Function ((&))
import Data.List (foldl')
import Data.List.PointedList.Circular
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V

day9 :: (Int, Int, Int) -> (Int, Int)
day9 (nPlayers, nMarblesA, nMarblesB) =
  let resp m = V.maximum (play nPlayers m)
   in (resp nMarblesA, resp nMarblesB)

put :: Int -> PointedList Int -> (Int, PointedList Int)
put x l =
  if x `mod` 23 == 0
    then let l' = moveN (-7) l
             marble = _focus l'
          in (marble + x, fromJust (deleteRight l'))
    else (0, (insertLeft x . moveN 2) l)

play :: Int -> Int -> V.Vector Int
play nPlayers nMarbles =
  fst . foldl' fld (V.replicate nPlayers 0, singleton 0) $ zip players marbles
  where
    fld (scores, circ) (p, m) =
      let (points, circ') = put m circ
       in (scores & ix p +~ points, circ')
    marbles = [1 .. nMarbles]
    players = cycle [0 .. nPlayers - 1]
