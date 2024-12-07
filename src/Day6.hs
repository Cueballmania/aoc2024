{-# LANGUAGE DeriveAnyClass #-}
module Day6 where

import System.IO
import Data.List

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)

type Position = (Int, Int)

type Map = [[Char]]

move :: Direction -> Position -> Position
move North (a,_) = (a-1, _)
move East  (_,b) = (_, b+1)
move South (a,_) = (a+1, _)
move West  (_,b) = (_, b-1)

canMove :: Map -> Direction -> Position -> Bool
canMove map d pos = if ch = '#' then False else True
    where ch = getChar map (move d pos)

getChar :: Map -> Position
getChar map pos = (Map!!(fst pos))!!(snd pos)

atEdge :: Position -> Int -> Bool
atEdge (0,_) _ = True
atEdge (_,0) _ = True
atEdge (x,y) m = if (x == m || y == m) then True else False

getPositions :: Map -> Direction -> Position -> [Position]

