{-# LANGUAGE PackageImports #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF (Font)

import Control.Monad
import "mtl" Control.Monad.State

import Data.Maybe
import Data.Char

import Network.Socket
import System.Environment

import Common
import DisplaySnake


initialise :: HostName -> String -> String -> Font -> IO ()
initialise hostname port name font = do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    let addr = addrAddress serveraddr
    sent <- sendTo sock ("L:" ++ name) addr
    flip evalStateT [] $ process sock addr font 

process :: Socket -> SockAddr -> Font -> StateT [Player] IO ()
process sock addr font = do 
    (str,_,saddr) <- liftIO $ recvFrom sock 1024
    case str of
        'W':idchr:wstr -> do
            let w = unPkgWorld wstr
                myid = ord idchr
            scores <- get
            liftIO $ drawWorld font w scores
        'S':sstr -> do
            let scores = unPkgScores sstr
            put scores
        _ -> liftIO $ putStrLn $ "Mystery message: " ++ str
    msg <- liftIO $ do 
        msg <- handleEvent 
        when (isJust msg) $ do
            sendTo sock (fromJust msg) addr 
            return ()
        return msg
    unless (msg == Just "Exit") $ process sock addr font
    liftIO SDL.quit
 where
    handleEvent :: IO (Maybe String)
    handleEvent = SDL.pollEvent >>= \e -> case e of
        SDL.KeyDown k -> case SDL.symKey k of 
                           SDL.SDLK_UP     -> return $ Just "U"
                           SDL.SDLK_DOWN   -> return $ Just "D"
                           SDL.SDLK_LEFT   -> return $ Just "L"
                           SDL.SDLK_RIGHT  -> return $ Just "R"
                           SDL.SDLK_SPACE  -> return $ Just "Play"
                           SDL.SDLK_ESCAPE -> return $ Just "Exit"
                           _               -> handleEvent 
        SDL.NoEvent -> return Nothing
        _ -> handleEvent

main :: IO ()
main = do
    [addr,port,name] <- getArgs
    font <- initGraphics
    SDL.enableEvent SDL.SDLMouseMotion False
    initialise addr port name font

