{-# LANGUAGE MultiWayIf #-}

module Days.Day15
  ( day15
  ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (guard)
import Data.List (foldl', scanl', sortBy)
import Data.Matrix hiding ((<|>), toList, trace)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Set as S

import Control.Exception.Assert
import Util

data Cave
  = Open
  | Wall
  deriving (Eq, Ord, Enum)

instance Show Cave where
  show Open = "."
  show Wall = "#"

data UnitType
  = Goblin
  | Elf
  deriving (Eq, Ord, Enum)

instance Show UnitType where
  show Elf = "E"
  show Goblin = "G"

data Unit = Unit
  { _typ :: UnitType
  , _attack :: Int
  , _hit :: Int
  , _moved :: Bool
  } deriving (Eq, Ord)

instance Show Unit where
  show u = show (_typ u)

makeLensesFor [("_hit", "hit"), ("_moved", "moved")] ''Unit

data Loc
  = C Cave
  | U Unit
  deriving (Eq, Ord)

instance Show Loc where
  show (C c) = show c
  show (U u) = show u

type Map = Matrix Loc

type Pos = (Int, Int)

getMap :: Int -> String -> Map
getMap ae input =
  let charToLoc c =
        case c of
          'E' -> U (Unit Elf ae 200 False)
          'G' -> U (Unit Goblin 3 200 False)
          '#' -> C Wall
          _ -> C Open
   in fromLists $ map (map charToLoc) $ lines input

getUnitsPos :: Map -> [Pos]
getUnitsPos m =
  map fst $
  filter
    ((/= Nothing) . snd)
    [ ((y, x), ut)
    | y <- [1 .. nrows m]
    , x <- [1 .. ncols m]
    , let ut = getUnit m (y, x)
    ]

byReading :: (Ord a) => (Pos -> a) -> [Pos] -> [Pos]
byReading f = sortBy cmp
  where
    cmp = comparing f <> comparing fst <> comparing snd

isOpen :: Map -> Pos -> Bool
isOpen m (y, x) =
  case getElem y x m of
    C Open -> True
    _ -> False

getUnit :: Map -> Pos -> Maybe Unit
getUnit m (y, x) =
  case getElem y x m of
    U u -> Just u
    _ -> Nothing

inRange :: Map -> Pos -> [Pos]
inRange m p = filter (isOpen m) $ neighbors4 p

move :: Map -> Pos -> Pos -> Maybe Map
move cur src dst = do
  u <- getUnit cur src
  guard $ isOpen cur dst
  return $ setElem (U u) dst cur & setElem (C Open) src

attack :: Map -> Pos -> Pos -> Maybe Map
attack cur src dst = do
  s <- getUnit cur src
  d <- getUnit cur dst
  let d' = assert (_typ s /= _typ d) $ d & hit `over` flip (-) (_attack s)
  return $
    if _hit d' > 0
      then setElem (U d') dst cur
      else setElem (C Open) dst cur

getTargetsPos :: Map -> UnitType -> [Pos]
getTargetsPos m ut =
  filter (maybe False ((/= ut) . _typ) . getUnit m) $ getUnitsPos m

mark :: Bool -> Pos -> Map -> Map
mark b p cur =
  fromMaybe cur $ do
    u <- getUnit cur p
    let u' = u & moved `set` b
    return $ setElem (U u') p cur

prepare :: Map -> Map
prepare m =
  let ps = getUnitsPos m
   in foldl' (flip $ mark False) m ps

attackM :: Map -> Pos -> Maybe (Map, Pos)
attackM cur p = do
  u <- getUnit cur p
  guard $ not $ _moved u
  let tgs = S.fromList $ getTargetsPos cur (_typ u)
      cand =
        byReading (fmap _hit . getUnit cur) $
        S.toList $ S.intersection tgs $ S.fromList $ neighbors4 p
  opp <-
    listToMaybe cand
  m <- assert (manhattan p opp == 1) $ attack cur p opp
  return (m, p)

nearEnemy :: Map -> (Int, Int) -> Unit -> Bool
nearEnemy m p u =
  fromMaybe False $ do
    c <- sequence $ filter isJust $ map (getUnit m) (neighbors4 p)
    return $ any ((/= _typ u) . _typ) c

search :: Unit -> Map -> S.Set Pos -> S.Set (Int, Pos, Pos) -> Maybe Pos
search u m seen s = do
  ((dist, dest, start), q) <- S.minView s
  if | S.member dest seen -> search u m seen q
     | nearEnemy m dest u -> Just start
     | otherwise -> search u m (S.insert dest seen)
                                 (foldl' (flip S.insert) q
                                     [(dist+1, dest', start) | dest' <- inRange m dest])

moveM :: Map -> Pos -> Maybe (Map, Pos)
moveM cur p = do
  u <- getUnit cur p
  guard $ not $ _moved u
  p' <- search u cur S.empty $ S.fromList [(0, start, start) | start <- inRange cur p]
  m <- assert (manhattan p p' == 1) $ move cur p p'
  return (m, p')

action :: Map -> Pos -> Map
action cur p =
  fromJust $ do
    let mv = moveM cur p
    (m, p') <-
      attackM cur p <|> (mv >>= uncurry attackM) <|> mv <|> return (cur, p)
    return $ mark True p' m

turn :: Map -> [Map]
turn cur = scanl' action (prepare cur) $ getUnitsPos cur

day15 :: Str -> (Int, Int)
day15 (Str input) =
  let m1 = getMap 3 input
      ec = length . filter ((== Elf) . _typ) . units $ m1
      units m = mapMaybe (getUnit m) (getUnitsPos m)
      isOver =
        (\a -> all ((== Goblin) . _typ) a || all ((== Elf) . _typ) a) . units
      battle m =
        let ms =
              break (any isOver) $ tail $ map init $ iterate (turn . last) [m]
         in fst ms ++ [head $ snd ms]
      outcome m =
        let a = last $ zip [0 ..] $ battle m
            points (n, m') = (n * sum (map _hit $ units m'), m')
         in points $ (\(n, ms) -> (n, last ms)) a
      outcome2 =
        head $
        dropWhile ((/= ec) . length . filter ((== Elf) . _typ) . units . snd) $
        fmap (\n -> outcome (getMap n input)) [4 ..]
      outcome1 = outcome m1
   in (fst outcome1, fst outcome2)
