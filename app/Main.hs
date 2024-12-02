module Main (main) where

import Day2
import Day1

main :: IO ()
main = 
    day1aMain >>
    day1bMain >>
    day2aMain >>
    day2bMain
