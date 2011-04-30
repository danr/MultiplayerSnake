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
    visited <- newArray b False :: ST s (STUArray s Pos Bool)
    let qinit = [(d,1,pos `addDir` d) | d <- take 4 $ drop (fromEnum dir) $ cycle [U,D,L,R]]
    queue <- newSTRef qinit
    forM_ qinit $ \(_,_,p) -> writeArray visited p True
    retry $ readSTRef queue >>= \q -> case q of
        [] -> return (True,Nothing)
        (d,s,p@(x,y)):q' 
            | p == bounty -> return (True,Just d) 
            | otherwise -> do
                writeSTRef queue q'
                e <- readArray a p
                when (e < s) $ forM_ [U,D,L,R] $ \d' -> do
                    let p' = p `addDir` d'
                    v <- readArray visited p'
                    writeArray visited p' True
                    unless v $ modifySTRef queue (++ [(d,s+1,p')])
                if null q' 
                    then return (True,Just d)
                    else return (False,Nothing)
                    

mkWorld :: Id -> World -> ST s (STUArray s Pos Int)
mkWorld i w@(World {..}) = do
    let w = width-1
        h = height-1
    a <- newArray ((0,0),(w,h)) 0
    forM_ [0..w] $ \x -> do
        writeArray a (x,0) maxBound
        writeArray a (x,h) maxBound
    forM_ [0..h] $ \y -> do
        writeArray a (0,y) maxBound
        writeArray a (w,y) maxBound
    mapM_ (writeSnake a i) snakes
    return a

writeSnake :: STUArray s Pos Int -> Id -> Snake -> ST s ()
writeSnake a i (Snake {..}) = do
    let b = reverse $ zip (reverse body) [1..]
    unless (i == snakeId) $
        forM_ [head body `addDir` d | d <- [U,D,L,R] ] $ \p -> do
            v <- readArray a p
            when (v == 0) $ writeArray a p 1
    mapM_ (uncurry (writeArray a)) b


