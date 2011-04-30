{-# LANGUAGE RecordWildCards,NamedFieldPuns,ViewPatterns #-}
module DisplaySnake where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as M
import Data.Map (Map)

import Graphics.UI.SDL.TTF (Font)

import Data.Bits
import Data.List
import Data.Function
import Control.Monad

import Common
import Text

type GLd = GL.GLdouble
type GLpos = (GLd,GLd)
type Color = (GLd,GLd,GLd)

winWidth,winHeight :: Int
winWidth  = 800
winHeight = 600
fontHeight = 24

colors :: Map Id Color
colors = M.fromList $ zip [0..] $ sortBy (compare `on` value) $ do
    [r,g,b] <- replicateM 3 [1,0,0.6]
    guard $ [r,g,b] /= [1,1,1] && [r,g,b] /= [0,0,0]
    return (r,g,b)
  where
    value (r,g,b) = v r + v g + v b 
    v 1   = 1
    v 0   = 0
    v 0.6 = 4

snakeIdToColor :: Id -> Color
snakeIdToColor = (colors M.!)

colorToSDLColor :: Color -> SDL.Color
colorToSDLColor (r,g,b) = SDL.Color (round $ 255.0 * r) 
                                    (round $ 255.0 * g) 
                                    (round $ 255.0 * b)

snakeIdToSDLColor :: Id -> SDL.Color
snakeIdToSDLColor = colorToSDLColor . snakeIdToColor

initGraphics :: IO Font
initGraphics = do
    SDL.init [SDL.InitVideo,SDL.InitTimer]
    SDL.setVideoMode winWidth winHeight 32 [SDL.OpenGL]
    initText "/usr/share/fonts/TTF/verdana.ttf" fontHeight

setupWorld :: World -> IO (GLpos,Pos)
setupWorld (World {width,height}) = do
    GL.loadIdentity
    GLU.ortho2D 0 (tod (winWidth - 1)) (tod (winHeight - 1)) 0
    GL.clear [GL.ColorBuffer,GL.DepthBuffer]
    let size = min (winWidth `div` width) (winHeight `div` height)
        hs = winHeight - size * height
        ws = winWidth - size * width 
        scb | hs > ws   = (0,size * height)
            | otherwise = (size * width,0)
    return ((tod size,tod size),scb)

setColor :: Color -> IO ()
setColor (r,g,b) = GL.color $ GL.Color3 r g b

drawWorld :: Font -> World -> [Player] -> IO ()
drawWorld font w@(World {..}) scores = do
    (sqsize,scb) <- setupWorld w
    setColor (0.6,0.6,0.6)
    forM_ [0..width-1] $ \x -> do
        drawBlock sqsize (x,0)
        drawBlock sqsize (x,height - 1)
    forM_ [0..height-1] $ \y -> do
        drawBlock sqsize (0,y)
        drawBlock sqsize (width - 1,y)
    forM_ snakes (drawSnake sqsize)
    setColor (1,1,1)
    drawBlock sqsize bounty
    drawScoreBoard font scb scores
    SDL.glSwapBuffers

drawScoreBoard :: Font -> Pos -> [Player] -> IO ()
drawScoreBoard font (x,y) scores = do
    let sorted = sortBy (flip compare `on` score) scores
    setupText
    forM_ (zip [0..] sorted) $ \(n,Player {..}) -> do
        let col = snakeIdToSDLColor playerId
        drawText font (show score) (x,y + n * (fontHeight + 4))                    col
        drawText font name         (x + (fontHeight * 3),y + n * (fontHeight + 4)) col
    unsetText

drawSnake :: GLpos -> Snake -> IO ()
drawSnake sqsize (Snake {..}) = do
    setColor (snakeIdToColor snakeId)
    forM_ body (drawBlock sqsize) 

drawBlock :: GLpos -> Pos -> IO ()
drawBlock (sx,sy) (tod -> x,tod -> y) = 
    GL.rect (GL.Vertex2 (sx * x) (sy * y))
            (GL.Vertex2 (sx * x + sx - 1) (sy * y + sy - 1))



