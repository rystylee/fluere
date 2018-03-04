module Sound.Fluere.Default where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Fluere.BaseData
import Sound.Fluere.Clock (newClock, newClockMMap)
import Sound.Fluere.Player (newPlayer, newPlayerMMap)
import Sound.Fluere.FluereWorld (newFluereWorld)


-- These functions are used to get a default data
--
defaultClock :: IO Clock
defaultClock = do
    let cname = "defaultClock"
        bpm   = 60 :: Double
        beat = 4 :: Double
        lasteventtime = 0 :: Double
        nexteventtime = 0 :: Double
    return $ newClock cname bpm beat lasteventtime nexteventtime

defaultPlayer :: IO Player
defaultPlayer = do
    let pname = "defaultPlayer"
        posc = [string "kick1", string "freq", float 440]
        pscore = [[1,0,1,0], [1,1,1,1]]
        pstatus = Pausing
    return $ newPlayer pname posc pscore pstatus

defaultFluereWorld :: IO FluereWorld
defaultFluereWorld = do
    cmmap <- defaultClockMMap
    pmmap <- defaultPlayerMMap
    return $ newFluereWorld "defaultWorld" cmmap pmmap
--
--

-- These functions are used to get a default MutableMaps
--
defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = do
    c <- defaultClock
    newClockMMap c

defaultPlayerMMap :: IO (TVar (Map String Player))
defaultPlayerMMap = do
    p <- defaultPlayer
    newPlayerMMap p
--
--
