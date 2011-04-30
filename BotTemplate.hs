{-# LANGUAGE RecordWildCards #-}
module BotTemplate 
    ( HandlerFunc  
    , runBot
    , getMe
    , getDir
    ) where

import Network.Socket
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char

import Common

-- main :: IO ()
-- main = do

type HandlerFunc = Id -> World -> Maybe Dir

runBot :: HandlerFunc -> IO ()
runBot h = do
    [addr,port,name] <- getArgs
    s <- initialise addr port name
    forever $ process s h
    
initialise :: HostName -> String -> Name -> IO (Socket,SockAddr)
initialise hostname port name = do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    let addr = addrAddress serveraddr
    sendTo sock ("L:" ++ name) addr
    return (sock,addr)

process :: (Socket,SockAddr) -> HandlerFunc -> IO ()
process (sock,addr) handle = do 
    (str,_,saddr) <- recvFrom sock 1024
    msg <- case str of
        'W':idchr:wstr -> do
            let world = unPkgWorld wstr
                idint = ord idchr
            if idint /= 255 && idint `elem` map snakeId (snakes world)
                then return $ show `fmap` handle idint world
                else return $ Just "Play"
        'S':sstr -> do
            let scores = unPkgScores sstr
            return Nothing
        _ -> do
            putStrLn $ "Mystery message: " ++ str
            return Nothing
    print msg
    when (isJust msg) (sendTo sock (fromJust msg) addr >> return ())

getMe :: Id -> World -> Snake
getMe i = fromMaybe (error "getMe") . find ((== i) . snakeId) . snakes

getDir :: Snake -> Maybe Dir
getDir (Snake {..}) 
    | length body >= 2 = posToDir $ head body - (head (tail body))
    | otherwise        = Nothing

