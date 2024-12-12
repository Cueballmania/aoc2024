module Day11 where

import System.IO
import Data.List
import qualified Data.Map

day11aMain :: IO ()
day11aMain = do
    file <- readFile "files/day11_input.txt"
    let stones = map (read :: String -> Int) $ words file
    print $ length $ blinkN 25 stones

blinkN :: Int -> [Int] -> [Int]
blinkN 0 xs = xs
blinkN n xs = blinkN (n-1) $ blink xs

blink :: [Int] -> [Int]
blink [] = []
blink (x:xs) = steps x ++ blink xs

steps :: Int -> [Int]
steps 0 = [1]
steps x = if (length . show $ x) `mod` 2 == 0 then split2 x else [x*2024]

split2 :: Int -> [Int]
split2 x = [(read :: String -> Int) $ take half strX] ++ 
           [(read :: String -> Int) $ drop half strX]
    where
    strX = (show x)
    half = (length strX) `div` 2

day11bMain :: IO ()
day11bMain = do
    file <- readFile "files/day11_input.txt"
    let stones = map (read :: String -> Int) $ words file
    let stoneMap = Data.Map.fromList [(val, 1) | val <- stones]
    print $ sum $ Data.Map.elems $ blinkMapN 75 stoneMap

blinkMapN :: Int -> Data.Map.Map Int Int -> Data.Map.Map Int Int
blinkMapN 0 bmap = bmap
blinkMapN n bmap = blinkMapN (n-1) $ blinkMap bmap

blinkMap :: Data.Map.Map Int Int -> Data.Map.Map Int Int
blinkMap bmap = Data.Map.fromListWith (+) [(a',b) | (a,b) <- Data.Map.toList bmap, a' <- steps a]
