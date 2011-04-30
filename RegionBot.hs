{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe
import Data.List
import Data.Char

import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.Array
import Data.STRef
import Data.Ord

import BotTemplate
import Common

main :: IO ()
main = runBot handle

handle :: Id -> World -> Maybe Dir
handle i w@(World {..}) = 
    let me@(Snake {..}) = getMe i w
        dir             = fromMaybe U (getDir me)
    in  case answer (head body) dir i w of
            Just dir' | dir == dir' -> Nothing
            x                       -> x

answer :: Pos -> Dir -> Id -> World -> Maybe Dir
answer pos dir i w@(World {..}) = runST $ do
    a <- mkWorld i w
    b <- getBounds a
    attract a bounty
    mapM_ (repel a . head . body) (filter ((/= i) . snakeId) snakes)
    vals <- forM [U,D,L,R] $ \d -> do
                vs <- forM [-2,-1,0,1,2] $ \x ->
                        forM [-2,-1,0,1,2] $ \y -> do
                            let p = (pos + (x,y)) `addDir` d `addDir` d
                            if inRange b p
                                then readArray a p
                                else return (-10)
                return (d,sum $ concat vs)
    let mydir = fst $ maximumBy (comparing snd) vals
    debug $ show vals
    debug $ show mydir
    if mydir == dir then return Nothing else return (Just mydir)
        
attract a = terrain a (*4) 
repel a = terrain a negate

terrain :: STUArray s Pos Int -> (Int -> Int) -> Pos -> ST s ()
terrain a f p = do
    b@((xl,yl),(xh,yh)) <- getBounds a
    --debug $ show b
    forM_ [xl..xh] $ \x -> 
        forM_ [yl..yh] $ \y -> do
            let dist = uncurry (+) $ abs $ (x,y) - p
                eff = f $ max (80 - dist) 0
            v <- readArray a p
            -- debug $ show ((x,y),eff)
            writeArray a p (eff + v)
    

mkWorld :: Id -> World -> ST s (STUArray s Pos Int)
mkWorld i w@(World {..}) = do
    let w = width-1
        h = height-1
    a <- newArray ((1,1),(w-1,h-1)) 0
    mapM_ (writeSnake a i) snakes
    return a

writeSnake :: STUArray s Pos Int -> Id -> Snake -> ST s ()
writeSnake a i (Snake {..}) = do
    mapM_ (flip (writeArray a) (-1)) body


