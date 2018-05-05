module Sound.Fluere.Core.TempoClock where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)

import Sound.Fluere.Core.Time (currentTime, physicalBar, physicalBeat, logicalTimeOfBeat, beatDelta, sleep)
import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, lookupM, insertM)
import Sound.Fluere.Core.BaseData
import Sound.Fluere.Core.Environment (getPlayerNames)
import Sound.Fluere.Core.Player (play)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newTempoClock :: String
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> TempoClock
newTempoClock n c b st sBar sBeat cl =
    TempoClock { tempoClockName = n
               , cps = c
               , bpc = b
               , startTime = st
               , startBar = sBar
               , startBeat = sBeat
               , clockLatency = cl
               }

newTempoClockMMap :: TempoClock -> IO (MutableMap String TempoClock)
newTempoClockMMap tc = fromListM [(tempoClockName tc, tc)]

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapTempoClock :: Environment -> String -> (TempoClock -> TempoClock) -> IO ()
swapTempoClock e n f = do
    let tcmmap = tempoClockMMap e
    Just tc <- lookupM n tcmmap
    let newtc = f tc
    insertM n newtc tcmmap

swapCps :: Environment -> String -> Double -> IO ()
swapCps e n c = do
    Just tc <- lookupM n $ tempoClockMMap e
    now <- currentTime
    pBar <- physicalBar tc
    pBeat <- physicalBeat tc
    let swapcps newtc = newtc { tempoClockName = n
                              , cps = c
                              , bpc = bpc tc
                              , startTime = now
                              , startBar = pBar
                              , startBeat = pBeat
                              , clockLatency = clockLatency tc
                              }
    swapTempoClock e n swapcps

swapBpc :: Environment -> String -> Double -> IO ()
swapBpc e n b = do
    Just tc <- lookupM n $ tempoClockMMap e
    now <- currentTime
    pBar <- physicalBar tc
    pBeat <- physicalBeat tc
    let swapbps newtc = newtc { tempoClockName = n
                              , cps = cps tc
                              , bpc = b
                              , startTime = now
                              , startBar = pBar
                              , startBeat = pBeat
                              , clockLatency = clockLatency tc
                              }
    swapTempoClock e n swapbps

-- alias
cps' = swapCps
bpc' = swapBpc

---------------------------------------------------------------------
-- tick loop
---------------------------------------------------------------------

startTick :: Environment -> String -> IO ()
startTick e n = do
    Just tc <- lookupM n $ tempoClockMMap e
    _ <- forkIO $ tickLoop e n
    return ()

tickLoop :: Environment -> String -> IO ()
tickLoop e n = do
    Just tc <- lookupM n $ tempoClockMMap e
    pb <- physicalBeat tc
    lb <- logicalTimeOfBeat tc pb
    let lt = lb + clockLatency tc
    processPlayers e lt
    sleep $ beatDelta tc - 0.01
    tickLoop e n

processPlayers :: Environment -> Double -> IO ()
processPlayers e lt = do
    pnames <- getPlayerNames e
    sequence_ $ map (\p -> forkIO $ play e p lt) pnames
