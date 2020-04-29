{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Days.Day15
  ( day15
  , day15Draw
  ) where

import           Control.Applicative         ((<|>))
import           Control.Lens                hiding ((#))
import           Control.Exception.Assert
import           Control.Monad               (guard)

import qualified Data.ByteString.Lazy as L (writeFile)
import           Data.List            (foldl', scanl', sortBy)
import           Data.Matrix          hiding (toList, trace, (<|>))
import           Data.Maybe
import           Data.Ord             (comparing)
import qualified Data.Set             as S

import           Diagrams.Backend.Rasterific
import qualified Diagrams.Prelude            as D
import           Diagrams.Prelude ((#))

import           Codec.Picture.ColorQuant (PaletteCreationMethod (..))
import           Util

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

type Diagram = D.QDiagram B D.V2 Double D.Any

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
  p' <-
    search u cur S.empty $
    S.fromList [(0, start, start) | start <- inRange cur p]
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

units :: Map -> [Unit]
units m = mapMaybe (getUnit m) (getUnitsPos m)

battle :: Map -> [[Map]]
battle m =
  let isOver =
        (\a -> all ((== Goblin) . _typ) a || all ((== Elf) . _typ) a) . units
      ms = break (any isOver) $ tail $ map init $ iterate (turn . last) [m]
   in fst ms ++ [head $ snd ms]

outcome :: Map -> (Int, Map)
outcome m =
  let b = last $ zip [0 ..] $ battle m
      points (n, m') = (n * sum (map _hit $ units m'), m')
   in points $ (\(n, ms) -> (n, last ms)) b

day15 :: Str -> (Int, Int)
day15 (Str input) =
  let m1 = getMap 3 input
      ec = length . filter ((== Elf) . _typ) . units $ m1
      outcome2 =
        head $
        dropWhile ((/= ec) . length . filter ((== Elf) . _typ) . units . snd) $
        fmap (\n -> outcome (getMap n input)) [4 ..]
      outcome1 = outcome m1
   in (fst outcome1, fst outcome2)

locToD :: Loc -> Diagram
locToD l =
  case l of
    C c ->
      D.rect 10 10 #
      D.fc
        (if c == Wall
           then D.gray
           else D.black)
    U u ->
      D.text (show $ _hit u) # D.fontSize 8 # D.bold <> D.circle 5 #
      D.fc
        (if _typ u == Goblin
           then D.red
           else D.green)

animGif ::
     FilePath
  -> D.SizeSpec D.V2 Double
  -> GifLooping
  -> Int
  -> [Diagram]
  -> IO ()
animGif outFile sz gOpts i ds =
  case rasterGif sz gOpts opts (map (,i) ds) of
    Right bs -> L.writeFile outFile bs
    Left e   -> putStrLn e
  where
    opts = PaletteOptions Uniform True 16

toGif :: String -> [Diagram] -> IO ()
toGif fp = animGif fp (D.mkWidth 1000) LoopingForever 10

mapToGif :: String -> Map -> IO ()
mapToGif fp m =
  toGif fp $
  fmap (D.vcat . fmap (D.hcat . fmap locToD) . toLists) $ map last $ battle m

day15Draw :: Str -> IO ()
day15Draw (Str input) = do
  let m1 = getMap 3 input
      m2 = getMap 16 input
  mapToGif "images/day15_1.gif" m1
  mapToGif "images/day15_2.gif" m2
