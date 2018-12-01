{-# LANGUAGE ExistentialQuantification #-}
module Util (
    Solution,
    mkDay,
    Str(..),
    pfactors,
    divisors,
    fibs,
    safeLu,
    count,
    nPerms,
    fileToStr,
    printSolution
) where

import Data.List
import qualified Data.Map.Strict as Map
import System.TimeIt
import Data.Numbers.Primes
import Control.Monad.State

data Solution = forall a b. (Show a, Eq a, Read b) => Solution (b, a, b -> a)
mkDay :: (Show a, Eq a, Read b, Applicative f) => (b -> a, f b, a) -> f Solution
mkDay (s, i, a) = (\x y z -> Solution (x, y, z)) <$> i <*> pure a <*> pure s

newtype Str = Str String
instance Read Str where
    readsPrec _ input = [(Str input, "")]

pfactors :: Int -> [Int]
pfactors n = factor n primes
  where
    factor n' (p:ps) 
        | p*p > n'        = [n']
        | n' `mod` p == 0 = p : factor (n' `div` p) (p:ps)
        | otherwise       =     factor n' ps
    factor _ [] = undefined

divisors :: Integral a => a -> [a]
divisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where
     limit = (floor.sqr) n
     sqr :: Integral a => a -> Double
     sqr = sqrt.fromIntegral

fibs :: [Integer]
fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 2)

safeLu :: Int -> [a] -> Maybe a
safeLu i a
    | (i >= 0) && (length a > i) = Just (a !! i)
    | otherwise = Nothing

count :: (Ord k, Num a, Foldable t) => t k -> [(k, a)]
count a = Map.toList $ foldr f Map.empty a
    where
    f k m = case Map.lookup k m of
        Nothing -> Map.insert k 1 m
        Just _  -> Map.adjust (+1) k m

choose :: Eq a => StateT [a] [] a
choose = StateT (\s -> s >>= \v -> return (v, delete v s))

nPerms :: Eq a => Int -> [a] -> [[a]]
nPerms n = evalStateT (replicateM n choose)

fileToStr :: FilePath -> IO Str
fileToStr fn = Str <$> readFile fn

printSolution :: Int -> Solution -> IO ()
printSolution number (Solution (i, a, s)) = do
        let sol = s i
        timeIt $ putStr $ "Day " ++ show number ++ ": " ++
            if sol == a then
                 show sol ++ ": "
            else "\x1b[31mExpected: " ++ show a ++ " Got: " ++ show sol ++ "\x1b[0m "               
