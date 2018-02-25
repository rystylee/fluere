module Sound.Pulse.PulseWorld where

import Control.Concurrent.STM
import Data.Map

import Sound.Pulse.Clock
import Sound.Pulse.Player
import Sound.Pulse.PulseMutableMap


-- Data definition
--
data PulseWorld = PulseWorld { worldName :: String
                              ,wClockPulseMMap :: IO (TVar (Map String Clock))
                              ,wPlayerPulseMMap ::IO (TVar (Map String Player))
                             }
--
--

-- These functions are used to create data with PulseWorld
--
newPulseWorld ::  String
               -> IO (TVar (Map String Clock))
               -> IO (TVar (Map String Player))
               -> PulseWorld
newPulseWorld wname cmmap pmmap =
    PulseWorld { worldName = wname
                ,wClockPulseMMap = cmmap
                ,wPlayerPulseMMap = pmmap
               }
--
--
