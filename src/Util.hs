{-# LANGUAGE ExistentialQuantification #-}
module Util (
    Solution,
    mkDay,
    Str(..),
    pfactors,
    divisors,
    fibs,
    count,
    nPerms,
    fileToStr,
    printSolution,
    firstRepeated
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.TimeIt
import Data.Numbers.Primes
import Control.Monad.State
import Safe

data Solution = forall a b. (Show a, Eq a, Read b) => Solution (b, a, b -> a)
mkDay :: (Show a, Eq a, Read b, Applicative f) => (b -> a, f b, a) -> f Solution
mkDay (s, i, a) = (\x y z -> Solution (x, y, z)) <$> i <*> pure a <*> pure s

newtype Str = Str String
instance Read Str where
    readsPrec _ input = [(Str input, "")]

printSolution :: Int -> Solution -> IO ()
printSolution number (Solution (i, a, s)) = do
        let sol = s i
        timeIt $ putStr $ "Day " ++ show number ++ ": " ++
            if sol == a then
                    show sol ++ ": "
            else "\x1b[31mExpected: " ++ show a ++ " Got: " ++ show sol ++ "\x1b[0m "

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

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated [] = Nothing
firstRepeated xs = let f (a, s) b = (b, Set.insert a s)
                       g = scanl f (head xs, Set.empty) (tail xs)
    in fst <$> headMay (dropWhile (\ (a, b) -> not $ Set.member a b) g)
