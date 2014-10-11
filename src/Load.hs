module Load where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as MIDILoad
import qualified Data.Map as Map
import Drums

import Data.Metrology
import Data.Metrology.SI
import Sound.MIDI.Metrology
import Sound.MIDI.Util
import Sound.MIDI.Tempo
import Sound.MIDI.Note
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

loadGame :: FilePath -> IO Game
loadGame mid = do
  f <- MIDILoad.fromFile mid
  return origin{ _events = loadDrums f }

loadDrums :: F.T -> Map.Map Rational [Thing]
loadDrums f = let
  Left (ttrk : mustrks) = attachUnits f
  tmap = makeTempoMap $ ttrk
  drumtrk :: MusicTrack NN.Rational E.T
  drumtrk = head $ filter (\trk -> trackName trk == Just "PART DRUMS") mustrks
  drumtrk' :: TimeTrack NN.Rational E.T
  drumtrk' = trackMusicToTime tmap drumtrk
  readThing :: E.T -> Maybe Thing
  readThing e = case isNoteOn e of
    Just note -> case V.fromPitch $ notePitch note of
      96 -> Just $ Unplayed Kick
      97 -> Just $ Unplayed Snare
      98 -> Just $ Unplayed Tom1
      99 -> Just $ Unplayed Tom2
      100 -> Just $ Unplayed Tom3
      _ -> Nothing
    _ -> Nothing
  thingtrk :: TimeTrack NN.Rational [Thing]
  thingtrk = RTB.collectCoincident $ RTB.mapMaybe readThing drumtrk'
  thinglist :: [(Rational, [Thing])]
  thinglist = ATB.toPairList $ ATB.mapTime NN.toNumber $ RTB.toAbsoluteEventList 0 $ RTB.mapTime (# Second) thingtrk
  in Map.fromList thinglist
