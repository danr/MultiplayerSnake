{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe
import Data.List
import Data.Char

import BotTemplate
import Common

main :: IO ()
main = runBot handle

handle :: Id -> World -> Maybe Dir
handle i w@(World {..}) = 
    let me@(Snake {..}) = getMe i w
        dir             = getDir me
    in case dir of
        Just d  -> answer (head body) d width height
        Nothing -> Nothing

answer :: Pos -> Dir -> Int -> Int -> Maybe Dir
answer (x,y) d w h 
    | x <= 10 && d == L = Just U
    | y <= 10 && d == U = Just R
    | x > w - 10 && d == R = Just D
    | y > h - 10 && d == D = Just L
    | otherwise = Nothing
    


