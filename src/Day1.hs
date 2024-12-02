module Day1 where

import System.IO
import Data.List

type Pairs = (Int, Int)

day1aMain :: IO ()
day1aMain = do
    infile <- readFile "files/day1_input.txt"
    let filelines = lines infile
    let lsPairs = mkPairs filelines
    let sortedLsPairs = sortedPairs lsPairs
    putStrLn $ show $ sum $ pairDiffs sortedLsPairs

mkPairs :: [String] -> [Pairs]
mkPairs lns = [mkInts $ words ln| ln <- lns]
    where mkInts [a,b] = (read a :: Int, read b :: Int)

sortedPairs :: [Pairs] -> [Pairs]
sortedPairs lsPairs = zip (sort as) (sort bs)
    where (as, bs) = unzip lsPairs

pairDiffs :: [Pairs] -> [Int]
pairDiffs ls = [diff ln | ln <- ls]
    where diff ln = abs $ (fst ln - snd ln)

day1bMain :: IO ()
day1bMain = do
    infile <- readFile "files/day1_input.txt"
    let filelines = lines infile
    let lsPairs = mkPairs filelines
    let (lLs, rLs) = unzip lsPairs
    let feqLs = leftToRight lLs rLs
    putStrLn $ show $ sum $ zipWith (*) feqLs lLs

leftToRight :: [Int] -> [Int] -> [Int]
leftToRight lLs rLs = [count item rLs | item <- lLs]
    where count i xs = length (filter (== i) xs)
