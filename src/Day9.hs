module Day9 where

import System.IO
import Data.List
import Data.Char

day9aMain :: IO()
day9aMain = do
    file <- readFile "files/day9_input.txt"
    let fileLine = filter (/='\n') file
    let (fileSizesStr, emptySizesStr) = partitionData fileLine
    let fileSizes = map digitToInt fileSizesStr
    let emptySizes = map digitToInt emptySizesStr
    print $ initialDisk fileLine


partitionData :: String -> (String,String)
partitionData = separateData ([],[])

separateData :: (String,String) -> String -> (String,String)
separateData (xs, ys) [] = (xs, ys)
separateData (xs, ys) (x:[]) = (xs ++ [x], ys)
separateData (xs, ys) (x:y:z) = separateData (xs ++ [x], ys ++ [y]) z

initialDisk :: String -> [String]
initialDisk str = mkDisk 0 str

mkDisk :: Int -> String -> [String]
mkDisk _ [] = []
mkDisk n (x:[]) = pattern n x
mkDisk n (x:y:z) = pattern n x ++ replicate (digitToInt y) " " ++ mkDisk (n+1) z


pattern :: Int -> Char -> [String]
pattern n ch = replicate (digitToInt ch) (show n)

