{-# LANGUAGE TemplateHaskell #-}
module Drums where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

data Pad
  = Kick
  | Snare
  | Hihat
  | Crash
  | Tom1
  | Tom2
  | Tom3
  | Ride
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Thing
  = Unplayed Pad
  | Played   Pad
  | Overhit  Pad
  deriving (Eq, Ord, Show, Read)

data Game = Game
  { _events   :: Map.Map Rational [Thing]
  , _position :: Rational
  , _score    :: Integer
  , _combo    :: Integer
  , _window   :: Rational
  } deriving (Eq, Ord, Show)

-- | @cut ord k@ removes any key @j@ for which @compare j k == ord@.
cut :: (Ord k) => Ordering -> k -> Map.Map k v -> Map.Map k v
cut EQ k m = Map.delete k m
cut LT k m = case Map.splitLookup k m of
  (_, eq, gt) -> maybe id (Map.insert k) eq gt
cut GT k m = case Map.splitLookup k m of
  (lt, eq, _) -> maybe id (Map.insert k) eq lt

-- | Updates the game state for when a pad has been hit by the player.
-- We look for an unplayed note of that pad to play; if one exists, the combo
-- and score go up and the note is marked played. Otherwise, an overhit is
-- logged, and the combo is reset to 0.
hit :: Pad -> Game -> Game
hit pad g = let
  hitWindow
    = cut LT (_position g - _window g)
    $ cut GT (_position g + _window g)
    $ _events g
  targets = [ t | (t, xs) <- Map.toList hitWindow, Unplayed pad `elem` xs ]
  targets' = sortBy (comparing $ \t -> abs $ t - _position g) targets
  addOverhit xs = Just $ Overhit pad : fromMaybe [] xs
  addPlay = fmap $ map $ \e -> case e of
    Unplayed p | p == pad -> Played p
    _                     -> e
  multiplier =
    if      _combo g < 10 then 1
    else if _combo g < 20 then 2
    else if _combo g < 30 then 3
    else                       4
  in case targets' of
    [] -> g
      { _events = Map.alter addOverhit (_position g) $ _events g
      , _combo  = 0
      }
    t : _ -> g
      { _events = Map.alter addPlay t $ _events g
      , _combo  = _combo g + 1
      , _score  = _score g + 100 * multiplier
      }

-- | Moves the game state forward in time.
-- We look for unplayed notes which can't be played anymore, and if there are
-- more than zero, the combo is reset to 0.
moveForward :: Rational -> Game -> Game
moveForward t g = let
  startMissed = _position g - _window g -- inclusive
  endMissed   = startMissed + t         -- exclusive
  missWindow
    = cut LT startMissed
    $ cut EQ endMissed
    $ cut GT endMissed
    $ _events g
  missedNumber = length
    [ () | (_, xs) <- Map.toList missWindow, Unplayed _ <- xs ]
  in g
    { _position = _position g + t
    , _combo = if missedNumber == 0 then _combo g else 0
    }

origin :: Game
origin = Game
  { _events = Map.empty
  , _position = 0
  , _score = 0
  , _combo = 0
  , _window = 1 / 10
  }
