module Days.Days (
    solutions,
) where

import Util
import qualified Data.Map.Strict as Map

day1 :: () -> ()
day1 = const ()

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
   (  1, mkDay (day1, return (), () ))]
