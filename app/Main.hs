module Main (main) where

import Day5
import Day4
import Day2
import Day1

main :: IO ()
main = 
    day1aMain >>
    day1bMain >>
    day2aMain >>
    day2bMain >>
    day4aMain >>
    day5aMain >>
    day5bMain
