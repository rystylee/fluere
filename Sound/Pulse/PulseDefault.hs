module Sound.Pulse.PulseDefault where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Pulse.PulseData
import Sound.Pulse.Clock (newClock, newClockPulseMMap)
import Sound.Pulse.Player (newPlayer, newPlayerPulseMMap)
import Sound.Pulse.PulseWorld (newPulseWorld)


-- These functions are used to get a default data
--
defaultClock :: Clock
defaultClock = newClock "defaultClock" 60 Stopped

defaultPlayer :: Player
defaultPlayer = newPlayer "defaultPlayer" Regular 60 [string "imp", string "freq", float 440] [[1,0,1,0], [1,1,1,1]] Pausing

defaultPulseWorld :: IO PulseWorld
defaultPulseWorld = do
    cmmap <- defaultClockMMap
    pmmap <- defaultPlayerMMap
    return $ newPulseWorld "defaultWorld" cmmap pmmap
--
--

-- These functions are used to get a default PulseMutableMaps
--
defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = newClockPulseMMap "defaultClock" defaultClock

defaultPlayerMMap :: IO (TVar (Map String Player)) 
defaultPlayerMMap = newPlayerPulseMMap "defaultPlayerMMap" defaultPlayer
--
--
