module Text where

-- used tutorials:
-- http://www.haskell.org/~pairwise/HOpenGL/HOpenGL.html
-- http://www.gamedev.net/community/forums/topic.asp?topic_id=221790

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.Rendering.OpenGL (($=))

import Control.Monad
import Common

drawText :: TTF.Font -> String -> Pos -> SDL.Color -> IO ()
drawText font msg (x,y) color = do
    surf <- TTF.renderTextBlended font msg color 
    let w = (2 ^) $ ceiling $ logBase 2 $ realToFrac $ max (SDL.surfaceGetWidth surf)
                                                           (SDL.surfaceGetHeight surf)
    sqsurf <- SDL.createRGBSurfaceEndian [SDL.SWSurface] w w 32
    SDL.blitSurface surf Nothing sqsurf Nothing
    sqpixels <- SDL.surfaceGetPixels sqsurf
    [texName] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just texName
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing),GL.Nearest)
    GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA' (GL.TextureSize2D (toEnum w) (toEnum w)) 0
                  (GL.PixelData GL.RGBA GL.UnsignedByte sqpixels)
    SDL.freeSurface surf
    SDL.freeSurface sqsurf
    GL.textureBinding GL.Texture2D $= Just texName
    GL.renderPrimitive GL.Quads $ forM_ [(0,0),(0,1),(1,1),(1,0)] $ \(px,py) -> do
        GL.texCoord $ GL.TexCoord2 (toEnum px) (toEnum py :: GL.GLint)
        GL.vertex   $ GL.Vertex2 (toEnum $ x + w * px) (toEnum $ y + w * py :: GL.GLint)
    GL.deleteObjectNames [texName]

setupText :: IO ()
setupText = GL.texture GL.Texture2D $= GL.Enabled

unsetText :: IO ()
unsetText = GL.texture GL.Texture2D $= GL.Disabled

initText :: String -> Int -> IO TTF.Font
initText font size = do
    -- GL.blend $= GL.Enabled
    -- GL.blendEquation $= GL.FuncAdd
    -- GL.blendFunc $= (GL.SrcColor, GL.OneMinusSrcAlpha)
    -- GL.combineRGB $= GL.Replace'
    -- GL.combineAlpha $= GL.Interpolate
    GL.textureFunction $= GL.Replace
    TTF.init
    TTF.openFont font size
