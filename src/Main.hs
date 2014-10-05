module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Img
import Data.Bits
import Foreign
import Foreign.C
import Control.Monad (when)

import qualified Drums as D

main :: IO ()
main = do
  0 <- SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo
  Img.imgInit [Img.InitPNG]
  window <- throwIfNull "Couldn't make window" $
    withCString "Hello, world!" $ \title -> do
      let undef = SDL.windowPosUndefined
      SDL.createWindow title undef undef 640 480 0
  rend <- throwIfNull "Couldn't make renderer" $
    SDL.createRenderer window (-1) SDL.rendererFlagAccelerated
  drawSensei rend
  doFrame $ \cont -> do
    e <- getEvent
    case e of
      Just evt -> case evt of
        QuitEvent {} -> Img.imgQuit >> SDL.quit
        _ -> cont
      Nothing -> cont

getEvent :: IO (Maybe SDL.Event)
getEvent = alloca $ \pevt -> do
  e <- pollEvent pevt
  if e == 1
    then fmap Just $ peek pevt
    else return Nothing

drawSensei :: SDL.Renderer -> IO ()
drawSensei rend = do
  Right tex <- Img.imgLoadTexture rend "chinaman.png"
  0 <- SDL.renderClear rend
  0 <- SDL.renderCopy rend tex nullPtr nullPtr
  SDL.renderPresent rend
  return ()

doFrame :: (IO () -> IO ()) -> IO ()
doFrame f = do
  start <- SDL.getTicks
  f $ do
    end <- SDL.getTicks
    let procTime = end - start
        frame = 1000 `quot` 60
    when (frame > procTime) $ SDL.delay $ frame - procTime
    doFrame f
