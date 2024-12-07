{-# LANGUAGE FlexibleContexts #-}
module Day5 where

import System.IO
import Data.List
import Data.List.Split (splitOn)

type Order = (Int, Int)

type Page = [Int]

pageOrdered :: Page -> [Order] -> Bool
pageOrdered [] _ = True
pageOrdered (x:xs) orders = isOrdered x xs orders && pageOrdered xs orders
    where isOrdered _ [] _ = True
          isOrdered x xs orders = all (==True) [(i,x) `notElem` orders |
                                                            i <- xs]
middle :: [Int] -> Int
middle [] = 0
middle (x:[]) = x
middle xs = middle $ tail . reverse . tail $ xs

parseOrders :: [String] -> [Order]
parseOrders lsStr = [convert o | o <- lsStr]

convert :: String -> Order
convert o = ((read :: String -> Int) $ head c, 
             (read :: String -> Int) $ c!!1)
    where c = splitOn "|" o

mkPages :: String -> Page
mkPages = map read .  splitOn ","

day5aMain :: IO ()
day5aMain = do
    file <- readFile "files/day5_input.txt"
    let inputs = splitOn ("\n\n") file
    let orders = parseOrders $ words $ head inputs
    let pages = map mkPages $ words $ (inputs)!!1
    let orderedPages =  map (flip pageOrdered orders) pages
    print $ sum $ [if pageOrdered p orders then middle p else 0| p <- pages]

sortBad :: [Order] -> Page -> Page
sortBad orders p = sortBy (\x y -> if (x,y) `elem` orders then LT else GT) p
   

day5bMain :: IO ()
day5bMain = do
    file <- readFile "files/day5_input.txt"
    let inputs = splitOn ("\n\n") file
    let orders = parseOrders $ words $ head inputs
    let pages = map mkPages $ words $ (inputs)!!1
    let orderedPages =  map (flip pageOrdered orders) pages
    print $ sum $ [if pageOrdered p orders then 0 else middle $ sortBad orders p| p <- pages]
   
