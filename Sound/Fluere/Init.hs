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
import Sound.Fluere.Core.Instrument (newInstrumentMMap, newInstrument, kick, snare, hihat)

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
    immap <- initInstrumentMMap
    return $ newEnvironment "Environment" tcmmap pmmap ammap ioimmap immap

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
    let l = length wl
        mf = 1
        d = 1.0
        wf = 0.6
        ts = (8,8)
        step = 16
        wl = weightList ts step wf
        pl = probabilityList mf d wl
    return $ newIOISet "kick" l mf d wf ts step wl pl 0

initIOISetMMap :: IO (MutableMap String IOISet)
initIOISetMMap = do
    ioi <- initIOISet
    newIOISetMMap ioi

---------------------------------------------------------------------
-- Instrument
---------------------------------------------------------------------

--initDrum1 :: IO [Instrument]
--initDrum1 = do
--    return $ [kick, snare, hihat]
--
--initInstrumentMMap :: IO (MutableMap String Instrument)
--initInstrumentMMap = do
--    is <- initDrum1
--    immap <- newInstrumentMMap $ head is
--    sequence_ $ Prelude.map (\i -> insertM (instrumentName i) i immap) is
--    return immap

initInstrument :: IO Instrument
initInstrument = do
    return kick

initInstrumentMMap :: IO (MutableMap String Instrument)
initInstrumentMMap = do
    i <- initInstrument
    newInstrumentMMap i
