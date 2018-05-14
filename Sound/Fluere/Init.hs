module Sound.Fluere.Init where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Sound.OSC.FD

import Sound.Fluere.Core.Time (currentTime)
import Sound.Fluere.Core.MutableMap (MutableMap, insertM)
import Sound.Fluere.Core.BaseData
import Sound.Fluere.Core.Environment (newEnvironment)
import Sound.Fluere.Core.TempoClock (newTempoClockMMap, newTempoClock)
import Sound.Fluere.Core.Player (newPlayerMMap, newPlayer)
import Sound.Fluere.Core.Action (newActionMMap, newAction)
import Sound.Fluere.Core.IOISet (newIOISetMMap, newIOISet)
import Sound.Fluere.Core.SynthDef (newSynthDefMMap, newSynthDef, kick, snare, closehihat)
import Sound.Fluere.Core.Density (newDensityMMap, newDensity, sinDensityMap)

import Sound.Fluere.Stochastic.MetricalWeight (weightList)
import Sound.Fluere.Stochastic.Probability (probabilityList)


---------------------------------------------------------------------
-- Environment
---------------------------------------------------------------------

initEnvironment :: IO Environment
initEnvironment = do
    tcmmap <- initTempoClockMMap
    pmmap <- initPlayerMMap
    ammap <- initActionMMap
    ioimmap <- initIOISetMMap
    sdmmap <- initSynthDefMMap
    dmmap <- initDensityMMap
    return $ newEnvironment "Environment" tcmmap pmmap ammap ioimmap sdmmap dmmap

---------------------------------------------------------------------
-- TempoClock
---------------------------------------------------------------------

clockLatency' :: Double
clockLatency' = 0.2

initTempoClockMMap :: IO (MutableMap String TempoClock)
initTempoClockMMap = do
    tc <- initTempoClock
    newTempoClockMMap tc

initTempoClock :: IO TempoClock
initTempoClock = do
    now <- currentTime
    return $ newTempoClock "TempoClock" 1 8 now 0 0 clockLatency'

---------------------------------------------------------------------
-- Player
---------------------------------------------------------------------

initPlayerMMap :: IO (MutableMap String Player)
initPlayerMMap = do
    p <- initPlayer
    newPlayerMMap p

initPlayer :: IO Player
initPlayer = do
    let n = "kick"
        pa = "kick"
        ioi = "kick"
        ps = Stopping
    return $ newPlayer n pa ioi ps

---------------------------------------------------------------------
-- Action
---------------------------------------------------------------------

initActionMMap :: IO (MutableMap String Action)
initActionMMap = do
    a <- initAction
    newActionMMap a

initAction :: IO Action
initAction = do
    return $ newAction (PlaySound "kick" "kick")

---------------------------------------------------------------------
-- IOISet
---------------------------------------------------------------------

initIOISet :: IO IOISet
initIOISet = do
    let l = length pl
        gd = "GlobalDensity"
        mf = 1
        d = 1.0
        wf = 0.6
        ts = (8,8)
        step = 16
        wl = weightList ts step wf
        pl = probabilityList mf d wl
    return $ newIOISet "kick" l gd mf d wf ts step wl pl 0

initIOISetMMap :: IO (MutableMap String IOISet)
initIOISetMMap = do
    ioi <- initIOISet
    newIOISetMMap ioi

---------------------------------------------------------------------
-- SynthDef
---------------------------------------------------------------------

initSynthDef :: IO SynthDef
initSynthDef = do
    return kick

initSynthDefMMap :: IO (MutableMap String SynthDef)
initSynthDefMMap = do
    s <- initSynthDef
    newSynthDefMMap s

---------------------------------------------------------------------
-- Density
---------------------------------------------------------------------

initDensityMMap :: IO (MutableMap String Density)
initDensityMMap = do
    d <- initDensity
    newDensityMMap d

initDensity :: IO Density
initDensity = do
    let dm = sinDensityMap 200
    return $ newDensity "GlobalDensity" 200 dm
