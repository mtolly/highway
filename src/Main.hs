module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Img
import Data.Bits
import Foreign
import Foreign.C
import Control.Monad (when)

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
  Right tex <- Img.imgLoadTexture rend "chinaman.png"
  0 <- SDL.renderClear rend
  0 <- SDL.renderCopy rend tex nullPtr nullPtr
  SDL.renderPresent rend
  alloca $ \pevt -> let
    update = do
      start <- SDL.getTicks
      e <- pollEvent pevt
      if e == 1
        then do
          evt <- peek pevt
          case evt of
            QuitEvent {} -> do
              Img.imgQuit
              SDL.quit
            _ -> wait start
        else wait start
    wait start = do
      end <- SDL.getTicks
      let procTime = end - start
      when (frame > procTime) $ SDL.delay $ frame - procTime
      update
    frame = 1000 `quot` 60 :: Word32
    in update

{-
data Event
  = Hit       (Either Drum Cymbal)
  | RollStart (Either Drum Cymbal)
  | RollEnd   (Either Drum Cymbal)
  | Choke     Cymbal
  | HihatF
  deriving (Eq, Ord, Show, Read)

data Drum
  = Kick
  | Snare
  | HihatC
  | HihatO
  | Tom1
  | Tom2
  | Tom3
  deriving (Eq, Ord, Show, Read)

data Cymbal
  = Crash
  | Ride
  deriving (Eq, Ord, Show, Read)
-}
