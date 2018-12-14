module Days.Day13
  ( day13
  ) where

import Control.Lens
import Data.Bifunctor (bimap)
import Data.List (sort, unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)

import Util

data Track
  = T1
  | T2
  | CR
  deriving (Show, Eq, Ord, Enum)

data Dir
  = R
  | D
  | L
  | U
  deriving (Show, Eq, Ord, Enum)

data Pos
  = T Track
  | C Dir
  deriving (Show, Eq, Ord)

data Cart = Cart
  { _dir :: Dir
  , _pos :: (Int, Int)
  , _turns :: Maybe Bool
  } deriving (Show, Eq)

instance Ord Cart where
  compare = comparing (view _2 . _pos) <> comparing (view _1 . _pos)

makeLenses ''Cart

charToPos :: Char -> Maybe Pos
charToPos a = M.lookup a m
  where
    m = M.fromList (zip "/\\+>v<^" [T T1, T T2, T CR, C R, C D, C L, C U])

lineToRow :: String -> [(Int, Pos)]
lineToRow l =
  map (bimap id fromJust) $
  filter ((/= Nothing) . snd) $ zip [0 ..] (map charToPos l)

linesToTracks :: [String] -> M.Map (Int, Int) Pos
linesToTracks ls =
  M.fromList $
  concat $
  zipWith (\y ps -> map (\(x, p) -> ((x, y), p)) ps) [0 ..] $ map lineToRow ls

mkTrack :: Pos -> Maybe Track
mkTrack (C _) = Nothing
mkTrack (T t) = Just t

mkCart :: (Int, Int) -> Pos -> Maybe Cart
mkCart p (C d) = Just $ Cart d p (Just False)
mkCart _ _ = Nothing

turnR :: Dir -> Dir
turnR d = head . tail $ dropWhile (/= d) [R .. U] ++ [R]

turnL :: Dir -> Dir
turnL d = head . tail $ dropWhile (/= d) [U,L .. R] ++ [U]

move :: Dir -> (Int, Int) -> (Int, Int)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move R (x, y) = (x + 1, y)
move L (x, y) = (x - 1, y)

turn :: Maybe Bool -> Track -> Dir -> (Dir, Maybe Bool)
turn tr t d
  | t == T1 =
    ( if d `elem` [U, D]
        then turnR d
        else turnL d
    , tr)
  | t == T2 =
    ( if d `elem` [U, D]
        then turnL d
        else turnR d
    , tr)
  | isNothing tr = (d, Just True)
  | tr == Just False = (turnL d, Nothing)
  | otherwise = (turnR d, Just False)

moveCart :: M.Map (Int, Int) Track -> Cart -> Cart
moveCart ts c =
  let t = M.lookup (_pos c) ts
      d = _dir c
      tr = _turns c
   in case t of
        Nothing -> c & pos `over` move d
        Just t' ->
          let (d', tr') = turn tr t' d
           in c & pos `over` move d' & dir `set` d' & turns `set` tr'

moveCarts :: (Cart -> Cart) -> ([Cart] -> [Cart]) -> [Cart] -> [[Cart]]
moveCarts f h cs =
  take (length cs) $ drop 1 $ map (uncurry (++)) $ iterate g ([], cs)
  where
    g (a, b) =
      let c = head b
       in if _pos c `elem` map _pos a
            then (h (c : a), tail b)
            else (h (f (head b) : a), tail b)

removeDuplicates :: [Cart] -> [Cart]
removeDuplicates cs =
  let dp = map fst $ filter ((> (1 :: Int)) . snd) $ count $ map _pos cs
   in filter (not . (`elem` dp) . _pos) cs

findCollision :: [[Cart]] -> (Int, Int)
findCollision cs =
  _pos . head . head $
  dropWhile (all (== (1 :: Int)) . map snd . count . map _pos) cs

lastCart :: [[[Cart]]] -> (Int, Int)
lastCart cs = _pos . head . head $ dropWhile ((> 1) . length) $ map last cs

day13 :: Str -> ((Int, Int), (Int, Int))
day13 (Str input) =
  let tracksWithCarts = linesToTracks $ lines input
      carts =
        M.elems $
        M.map fromJust $
        M.filter (/= Nothing) $ M.mapWithKey mkCart tracksWithCarts
      tracks =
        M.map fromJust $ M.filter (/= Nothing) $ M.map mkTrack tracksWithCarts
      it = moveCarts $ moveCart tracks
      crts = unfoldr f $ sort carts
        where
          f cs =
            let css = it id cs
             in Just (css, sort $ last css)
      crts' = unfoldr f $ sort carts
        where
          f cs =
            let css = it removeDuplicates cs
             in Just (css, sort $ last css)
   in (findCollision $ concat crts, lastCart crts')
