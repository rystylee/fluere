module Sound.Pulse.PulseWorld where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Pulse.PulseData


-- These functions are used to create data with PulseWorld
--
newPulseWorld ::  String
               -> TVar (Map String Clock)
               -> TVar (Map String Player)
               -> PulseWorld
newPulseWorld wname cmmap pmmap =
    PulseWorld { worldName = wname
                ,wClockPulseMMap = cmmap
                ,wPlayerPulseMMap = pmmap
               }
--
--
