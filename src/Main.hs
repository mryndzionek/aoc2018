module Main where

import qualified Data.Map.Strict as Map

import Control.Applicative
import qualified Control.Exception as Ex

import System.Environment
import System.TimeIt

import Days.Days
import Safe
import Util

mayFile :: FilePath -> IO (Maybe String)
mayFile fp = do
  res <- Ex.try (readFile fp) :: IO (Either Ex.SomeException String)
  case res of
    Right contents -> return $ Just contents
    Left _ -> return Nothing

getInput :: String -> IO (Maybe String)
getInput i = return number <|> mayFile i <|> return (Just i)
  where
    number = show <$> (readMay i :: Maybe Int)

handle :: [String] -> IO ()
handle (a:_) =
  case Map.lookup p solutions of
    Just s -> s >>= printSolution p
    Nothing -> putStrLn "Day not yet completed !!!"
  where
    p = read a :: Int
handle [] = do
  mapM_ (\(n, s) -> s >>= printSolution n) $ Map.toList solutions
  timeIt $ putStr "Extras: " >> extras

main :: IO ()
main = do
  args <- getArgs
  handle args
