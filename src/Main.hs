module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
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

  let poll :: D.Game -> (D.Game -> IO ()) -> IO ()
      poll g cont = do
        e <- getEvent
        case e of
          Just evt -> case evt of
            SDL.QuitEvent {} -> Img.imgQuit >> SDL.quit
            SDL.KeyboardEvent {}
              | SDL.eventType evt == SDL.eventTypeKeyDown
              && SDL.keyboardEventRepeat evt == 0
              -> do
                cstr <- SDL.getScancodeName $ SDL.keysymScancode $ SDL.keyboardEventKeysym evt
                str <- peekCString cstr
                case lookup str keyPads of
                  Just p  -> poll (D.hit p g) cont
                  Nothing -> poll g cont
            _ -> poll g cont
          Nothing -> cont g

      keyPads :: [(String, D.Pad)]
      keyPads =
        [ ("V", D.Snare)
        , ("B", D.Tom1)
        , ("N", D.Tom2)
        , ("M", D.Tom3)
        , ("G", D.Hihat)
        , ("H", D.Crash)
        , ("J", D.Ride)
        , ("Space", D.Kick)
        ]

      draw :: D.Game -> IO ()
      draw _ = return ()

      doFrame :: D.Game -> Word32 -> IO ()
      doFrame g t = do
        poll g $ \g' -> do
          draw g'
          waitFrame t
          t' <- SDL.getTicks
          let g'' = D.moveForward (fromIntegral (t' - t) / 1000) g'
          doFrame g'' t'

  SDL.getTicks >>= doFrame D.origin

getEvent :: IO (Maybe SDL.Event)
getEvent = alloca $ \pevt -> do
  e <- SDL.pollEvent pevt
  if e == 1
    then fmap Just $ peek pevt
    else return Nothing

drawSensei :: SDL.Renderer -> IO ()
drawSensei rend = do
  Right tex <- Img.imgLoadTexture rend "chinaman.png"
  0 <- SDL.renderClear rend
  0 <- SDL.renderCopy rend tex nullPtr nullPtr
  SDL.renderPresent rend

waitFrame :: Word32 -> IO ()
waitFrame start = do
  end <- SDL.getTicks
  let procTime = end - start
      frame = 1000 `quot` 60
  when (frame > procTime) $ SDL.delay $ frame - procTime
