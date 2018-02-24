module Sound.Pulse.PulseDefault where

import Control.Concurrent.STM
import Data.Map
import Sound.OSC.FD (Datum, string, float)

import Sound.Pulse.Clock
import Sound.Pulse.Player
import Sound.Pulse.PulseWorld
import Sound.Pulse.PulseMutableMap


-- These functions are used to get a default data
--
defaultClock :: IO Clock
defaultClock = newClock "defaultClock" 60 Stopped

defaultPlayer :: IO Player
defaultPlayer = newPlayer "defaultPlayer" Regular 60 [string "imp", string "freq", float 440] [[1,0,1,0], [1,1,1,1]] Pausing

defaultPulseWorld :: IO PulseWorld
defaultPulseWorld = newPulseWorld "defaultWorld" defaultClockMMap defaultPlayerMMap
--
--

-- These functions are used to get a default PulseMutableMaps
--
defaultClockMMap = newClockPulseMMap "defaultClock" defaultClock

--defaultPlayerMMap :: IO (TVar Map k (IO Player)) 
defaultPlayerMMap = newPlayerPulseMMap "defaultPlayerMMap" defaultPlayer
--
--
