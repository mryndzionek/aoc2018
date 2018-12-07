module Days.Day3
  ( day3
  , day3Draw
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

import Control.Exception.Assert
import Data.Maybe (fromMaybe)
import Util

import Data.Colour.Palette.BrewerSet
import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (image)

type Claim = (Int, Int, Int, Int, Int)

getClaims :: Str -> [Claim]
getClaims (Str input) =
  let clearout =
        map
          (\c ->
             if c `elem` "@:x"
               then ','
               else c) .
        filter (`notElem` " #")
      parse s = read $ "(" ++ s ++ ")"
   in parse . clearout <$> lines input

day3 :: Str -> (Int, Int)
day3 input =
  let claims = getClaims input
      overlappingMap :: Map.Map (Int, Int) Integer
      overlappingMap = foldl' addArea Map.empty claims
        where
          addArea m c =
            foldl' (\m' p' -> Map.insertWith (+) p' 1 m') m $ claimToCords c
      overlappingArea = length $ Map.filter (> 1) overlappingMap
      claimToCords (_, x, y, w, h) =
        [(x', y') | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1]]
      checkOverlap c =
        all (== 1) $
        map (fromMaybe 0 . (`Map.lookup` overlappingMap)) $ claimToCords c
      (noOverlap, _, _, _, _) =
        let candidates = filter checkOverlap claims
         in byPred
              assert
              "Only one candidate available"
              ((1 ==) . length)
              candidates
              (head candidates)
   in (overlappingArea, noOverlap)

day3Draw :: Str -> IO ()
day3Draw input =
  let claims = getClaims input
      (_, no) = day3 input
      toSquare :: Claim -> (P2 Double, QDiagram B V2 Double Any)
      toSquare (i, x, y, w, h) =
        let c =
              if i == no
                then red
                else colors !! fromIntegral (rem i 8)
         in ( p2 (fromIntegral x, fromIntegral y)
            , rect (fromIntegral w) (fromIntegral h) # lw none # fc c)
      colors = brewerSet Set2 8
      example = position (map toSquare claims) # bg grey
   in renderRasterific
        "images/day3.png"
        (dims2D 1000 1000)
        (example :: Diagram B)
