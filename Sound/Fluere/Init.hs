module Sound.Fluere.Init where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Sound.OSC.FD

import Sound.Fluere.Data
import Sound.Fluere.DataBase (newDataBase)
import Sound.Fluere.MutableMap (MutableMap)
import Sound.Fluere.TempoClock (newTempoClockMMap, newTempoClock, currentTime)
import Sound.Fluere.Player (newPlayerMMap, newPlayer)
import Sound.Fluere.Action (newActionMMap, newAction)
import Sound.Fluere.Pattern (newPatternMMap, newPattern)
import Sound.Rhythm.ValueMap (newNoiseMap)


initDataBase :: IO DataBase
initDataBase = do
    tcmmap <- initTempoClockMMap
    plmmap <- initPlayerMMap
    ammap <- initActionMMap
    pammap <- initPatternMMap
    return $ newDataBase tcmmap plmmap ammap pammap

clockLatency' :: Double
clockLatency' = 0.2

initTempoClockMMap :: IO (MutableMap String TempoClock)
initTempoClockMMap = do
    tc <- initTempoClock
    newTempoClockMMap tc

initTempoClock :: IO TempoClock
initTempoClock = do
    now <- currentTime
    return $ newTempoClock "TempoClock" 0.5 4 now 0 0 clockLatency'

initPlayerMMap :: IO (MutableMap String Player)
initPlayerMMap = do
    p <- initPlayer
    newPlayerMMap p

initPlayer :: IO Player
initPlayer = do
    let n = "kick"
        pa = "kick"
        pp = "kick"
        ps = Playing
    return $ newPlayer n pa pp ps

initActionMMap :: IO (MutableMap String Action)
initActionMMap = do
    a <- initAction
    newActionMMap a

initAction :: IO Action
initAction = do
    let n = "kick"
        om = OscScLang { path = "/s_new"
                       , scMessage = [string "kick", string "freq", float 440]
                       }
    return $ newAction (PlaySound n om)

initPatternMMap :: IO (MutableMap String Pattern)
initPatternMMap = do
    p <- initPattern
    newPatternMMap p

initPattern :: IO Pattern
initPattern = do
    let n = "kick"
    nm <- newNoiseMap 10 (1,4)
    return $ newPattern n nm
