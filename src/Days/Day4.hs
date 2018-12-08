module Days.Day4
  ( day4
  ) where

import Data.Function (on)
import Data.List (foldl')
import qualified Data.List.Safe as S
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format

import Util

data Event
  = WakesUp
  | FallsAsleep
  | NewGuard Integer
  deriving (Show, Eq, Ord)

data Entry = Entry
  { date :: UTCTime
  , event :: Event
  } deriving (Show, Eq, Ord)

stats :: [Integer] -> [(Integer, Integer)]
stats = count

-- somewhat ugly, but time library in this resolver doesn't provide its own
timeToMinutes :: (Integral c, Read c, Show a) => a -> c
timeToMinutes = (`div` 60) . read . init . show

parseRecord :: String -> Maybe Entry
parseRecord line =
  let l = splitOn " " line
      idx = (S.!!) :: [a] -> Integer -> Maybe a
      date' =
        concat <$> sequence [l `idx` 0, pure " ", l `idx` 1] >>=
        parseTimeM True defaultTimeLocale "[%Y-%m-%d %H:%M]"
      event' =
        case l `idx` 2 of
          Just "wakes" -> pure WakesUp
          Just "falls" -> pure FallsAsleep
          Just "Guard" -> (NewGuard <$> read) . drop 1 <$> (l `idx` 3)
          _ -> Nothing
   in Entry <$> date' <*> event'

day4 :: Str -> (Maybe Integer, Maybe Integer)
day4 (Str input) =
  let entries = S.sort <$> traverse parseRecord (lines input)
      accum ((gid, d), m) e =
        case event e of
          NewGuard id' -> ((id', date e), m)
          FallsAsleep -> ((gid, date e), m)
          WakesUp ->
            ( (gid, date e)
            , Map.insertWith
                (++)
                gid
                [ ( timeToMinutes $ utctDayTime d
                  , timeToMinutes (diffUTCTime (date e) d))
                ]
                m)
      entriesMap =
        snd . foldl' accum ((0, UTCTime (ModifiedJulianDay 0) 0), Map.empty) <$>
        entries
      findMaxOn f = head . S.sortBy (flip compare `on` f)
      id1 = fst . findMaxOn (sum . map snd . snd) . Map.toList <$> entriesMap
      getMaxMinute = (findMaxOn snd <$> stats) . unfold
      sleepMaxMinute =
        getMaxMinute <$> (id1 >>= (\i -> entriesMap >>= Map.lookup i))
      unfold = concatMap (\(a, b) -> [a .. a + b - 1])
      strategy1Res = (*) <$> id1 <*> (fst <$> sleepMaxMinute)
      strategy2Res =
        (\(a, b) -> a * fst b) .
        findMaxOn (snd . snd) . Map.toList . Map.mapMaybe (pure . getMaxMinute) <$>
        entriesMap
   in (strategy1Res, strategy2Res)
