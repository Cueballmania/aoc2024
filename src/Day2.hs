module Day2 where

import System.IO
import Data.List

type Report = [Int]

day2aMain :: IO ()
day2aMain = do
    infile <- readFile "files/day2_input.txt"
    let filelines = lines infile
    let lsReport = mkReports filelines
    let incDecLs = intDec lsReport
    putStrLn $ show $ length (filter (==True) incDecLs)

mkReports :: [String] -> [Report]
mkReports lns = [map (read :: String -> Int)  $ words ln| ln <- lns]

intDec :: [Report] -> [Bool]
intDec xs = [ckIncDec x | x <- xs]

ckIncDec :: Report -> Bool
ckIncDec x = ckRun (+) x || ckRun (-) x

ckRun :: (Int -> Int -> Int) -> Report -> Bool
ckRun op [] = True
ckRun op (x:[]) = True
ckRun op (x:xs) = (((op x 1) == head xs) || ((op x 2) == head xs)
        || ((op x 3) == head xs)) && ckRun op xs

intDec' :: [Report] -> [Bool]
intDec' xs = [ckIncDec' x | x <- xs]

ckIncDec' :: Report -> Bool
ckIncDec' x = ckIncDec x || any (==True) 
                [ckIncDec $ removeNth n x | n <- [0..length x]]

removeNth :: Int -> Report -> Report
removeNth _ [] = []
removeNth n xs = take(n-1) xs ++ drop n xs

day2bMain :: IO ()
day2bMain = do
    infile <- readFile "files/day2_input.txt"
    let filelines = lines infile
    let lsReport = mkReports filelines
    let incDecLs = intDec' lsReport
    putStrLn $ show $ length (filter (==True) incDecLs)


