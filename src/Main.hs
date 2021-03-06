module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import Data.Bits
import Foreign
import Foreign.C
import Control.Monad (when, forM_)
import qualified Data.Map as Map

import qualified Drums as D
import qualified Load

import qualified Sound.OpenAL as AL
import Sound.OpenAL (($=))
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV
import qualified Data.Vector.Storable as V

main :: IO ()
main = do

  srcL <- AL.genObjectName
  srcR <- AL.genObjectName
  AL.sourcePosition srcL $= AL.Vertex3 (-1) 0 0
  AL.sourcePosition srcR $= AL.Vertex3 1    0 0
  hsong <- Snd.openFile "song.ogg" Snd.ReadMode Snd.defaultInfo

  -- For now, just load upfront
  (_, Just buf) <- Snd.hGetContents hsong
  Snd.hClose hsong
  let vect = SndV.fromBuffer buf :: V.Vector Int16

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
        e <- pollEvent
        case e of
          Just evt -> case evt of
            SDL.QuitEvent {} -> print g >> Img.imgQuit >> SDL.quit
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
      draw game = do
        let xPos pad = fromIntegral $ 50 * fromEnum pad
            yPos secs = (480 -) $ round $ 200 + (secs - D._position game) * 300
        0 <- SDL.setRenderDrawColor rend 0 0 0 255
        0 <- SDL.renderClear rend
        0 <- SDL.setRenderDrawColor rend 255 255 255 255
        renderFillRect rend $ SDL.Rect 0 (yPos $ D._position game) 480 10
        forM_ (Map.toList $ D._events game) $ \(secs, things) ->
          forM_ things $ \thing -> do
            let thingPad = case thing of
                  D.Unplayed p   -> p
                  D.Played   p _ -> p
                  D.Overhit  p   -> p
                (r, g, b) = case thing of
                  D.Unplayed {} -> (255, 0, 0)
                  D.Played   {} -> (0, 255, 0)
                  D.Overhit  {} -> (0, 0, 255)
            0 <- SDL.setRenderDrawColor rend r g b 255
            renderFillRect rend $ SDL.Rect (xPos thingPad) (yPos secs) 10 10
        0 <- SDL.setRenderDrawColor rend 255 255 0 255
        forM_ [1 .. D._combo game] $ \n ->
          renderFillRect rend $ SDL.Rect (fromIntegral n * 10) 10 8 8
        SDL.renderPresent rend

      doFrame :: D.Game -> Word32 -> IO ()
      doFrame g t = do
        poll g $ \g' -> do
          draw g'
          waitFrame t
          t' <- SDL.getTicks
          let g'' = D.moveForward (fromIntegral (t' - t) / 1000) g'
          doFrame g'' t'

  game <- Load.loadGame "notes.mid"
  SDL.getTicks >>= doFrame game

renderFillRect :: SDL.Renderer -> SDL.Rect -> IO ()
renderFillRect rend rect = alloca $ \prect -> do
  poke prect rect
  0 <- SDL.renderFillRect rend prect
  return ()

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> do
  e <- SDL.pollEvent pevt
  if e == 1
    then fmap Just $ peek pevt
    else return Nothing

drawSensei :: SDL.Renderer -> IO ()
drawSensei rend = do
  Right tex <- Img.imgLoadTexture rend "img/sensei.png"
  0 <- SDL.renderClear rend
  0 <- SDL.renderCopy rend tex nullPtr nullPtr
  SDL.renderPresent rend

waitFrame :: Word32 -> IO ()
waitFrame start = do
  end <- SDL.getTicks
  let procTime = end - start
      frame = 1000 `quot` 60
  when (frame > procTime) $ SDL.delay $ frame - procTime
