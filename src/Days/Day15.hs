module Days.Day15
  ( day15
  ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (guard)
import Control.Parallel.Strategies
import Data.Function (on)
import Data.Graph.AStar
import qualified Data.HashSet as HS
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

getMap :: String -> Map
getMap input =
  let charToLoc c =
        case c of
          'E' -> U (Unit Elf 3 200 False)
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

byReading :: (a -> Pos) -> [a] -> [a]
byReading f = sortBy (cmp `on` f)
  where
    cmp = comparing fst <> comparing snd

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
inRange m p = filter (\(y, x) -> isOpen m (y, x)) $ neighbors4 p

shortestPath :: Map -> Pos -> Pos -> Maybe [Pos]
shortestPath m pa pb =
  aStar (HS.fromList . inRange m) manhattan (manhattan pa) (== pb) pa

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
        sortBy (compare `on` (fmap _hit . getUnit cur)) $
        S.toList $ S.intersection tgs $ S.fromList $ neighbors4 p
  lh <- _hit <$> (listToMaybe cand >>= getUnit cur)
  opp <-
    listToMaybe $
    byReading id $ takeWhile (maybe False ((== lh) . _hit) . getUnit cur) cand
  m <- assert (manhattan p opp == 1) $ attack cur p opp
  return (m, p)

moveM :: Map -> Pos -> Maybe (Map, Pos)
moveM cur p = do
  u <- getUnit cur p
  guard $ not $ _moved u
  let tgs = S.fromList $ getTargetsPos cur (_typ u)
      inr =
        S.toList $ foldl' S.union S.empty $ S.map (S.fromList . inRange cur) tgs
      pths =
        sortBy (compare `on` length) $
        catMaybes $ parMap rdeepseq (shortestPath cur p) inr
  p' <-
    listToMaybe $
    byReading id
    (head <$> takeWhile ((== (length $ head pths)) . length) pths)
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

day15 :: Str -> Int
day15 (Str input) =
  let m1 = getMap input
      units m = mapMaybe (getUnit m) (getUnitsPos m)
      isOver =
        (\a -> all ((== Goblin) . _typ) a || all ((== Elf) . _typ) a) . units
      battle m =
        let ms = break (any isOver) $ iterate (turn . last) [m]
         in fst ms ++ [head $ snd ms]
      isFull ms = length ms == length (takeWhile (not . isOver) ms) + 1
      outcome (n, m) = n * sum (map _hit $ units m)
      outcome1 =
        outcome .
        (\(n, ms) ->
           if isFull ms
             then (n, last ms)
             else (n - 1, last ms)) .
        last $
        zip [0 ..] $ battle m1
   in outcome1
