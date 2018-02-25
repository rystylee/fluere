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
defaultClock :: Clock
defaultClock = newClock "defaultClock" 60 Stopped

defaultPlayer :: Player
defaultPlayer = newPlayer "defaultPlayer" Regular 60 [string "imp", string "freq", float 440] [[1,0,1,0], [1,1,1,1]] Pausing

defaultPulseWorld :: IO PulseWorld
defaultPulseWorld =
    return $ newPulseWorld "defaultWorld" defaultClockMMap defaultPlayerMMap
--
--

-- These functions are used to get a default PulseMutableMaps
--
defaultClockMMap = newClockPulseMMap "defaultClock" defaultClock

defaultPlayerMMap :: IO (TVar (Map String Player)) 
defaultPlayerMMap = newPlayerPulseMMap "defaultPlayerMMap" defaultPlayer
--
--
