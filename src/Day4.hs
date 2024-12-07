module Day4 where

import System.IO
import Data.List (tails, transpose, isPrefixOf)

rotates :: [[a]] -> [[[a]]]
rotates = take 4 . iterate rotate
    where rotate = reverse . transpose

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals arr@(_:more) = transpose (zipWith drop [0..] arr) <> diagonals more

searchGrids :: [[a]] -> [[a]]
searchGrids xs = do
    grid <- rotates xs
    (tails =<< grid) <> diagonals grid

day4aMain :: IO ()
day4aMain = do
    file <- readFile "files/day4_input.txt"
    print $ (length . filter ("XMAS" `isPrefixOf`) . searchGrids) $ words file
