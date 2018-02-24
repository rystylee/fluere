module Sound.Pulse.PulseWorld where

import Control.Concurrent.STM
import Data.Map

import Sound.Pulse.Clock
import Sound.Pulse.Player
import Sound.Pulse.PulseMutableMap


data PulseWorld = PulseWorld { worldName :: String
                              ,wClockPulseMMap :: IO (TVar (Map String (IO Clock)))
                              ,wPlayerPulseMMap :: IO (TVar (Map String (IO Player)))
                             }


newPulseWorld ::  String
               -> IO (TVar (Map String (IO Clock)))
               -> IO (TVar (Map String (IO Player)))
               -> IO PulseWorld
newPulseWorld wname cmmap pmmap = do
    let world = PulseWorld { worldName = wname
                            ,wClockPulseMMap = cmmap
                            ,wPlayerPulseMMap = pmmap
                           }
    return world

getClock :: IO ()
getClock world = do
    w <- world
    keys $ wClockPulseMMap w
