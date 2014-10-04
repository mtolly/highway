module Main where

import Graphics.UI.SDL as SDL
import Data.Bits
import Foreign.C
import Foreign.Marshal

main :: IO ()
main = do
  0 <- SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo
  window <- throwIfNull "Couldn't make window" $
    withCString "Hello, world!" $ \str -> do
      let undef = SDL.windowPosUndefined
      SDL.createWindow str undef undef 640 480 0
  SDL.delay 3000
  SDL.quit
