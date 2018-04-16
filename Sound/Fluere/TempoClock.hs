module Sound.Fluere.TempoClock ( newTempoClock
                               , newTempoClockMMap
                               , cps'
                               , startTick
                               , getDensity
                               , currentTime
                               ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)

import Sound.Fluere.Data
import Sound.Fluere.DataBase (getPlayerNames, getPlayers)
import Sound.Fluere.MutableMap (MutableMap, fromListM, lookupM, insertM)
import Sound.Fluere.Player (play)

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
newTempoClock n c bps st sBar sBeat cl =
    TempoClock { tempoClockName = n
               , cps = c
               , beatsPerCycle = bps
               , startTime = st
               , startBar = sBar
               , startBeat = sBeat
               , clockLatency = cl
               }

newTempoClockMMap :: TempoClock -> IO (MutableMap String TempoClock)
newTempoClockMMap tc = fromListM [("TempoClock", tc)]

---------------------------------------------------------------------
-- Modify
---------------------------------------------------------------------

modifyTempoClock :: DataBase -> (TempoClock -> TempoClock) -> IO ()
modifyTempoClock db f = do
    let tcmmap = tempoClockMMap db
    Just tc <- lookupM "TempoClock" tcmmap
    let newtc = f tc
    insertM "TempoClock" newtc tcmmap

modifyCps :: DataBase -> Double -> IO ()
modifyCps db c = do
    Just tc <- lookupM "TempoClock" $ tempoClockMMap db
    now <- currentTime
    pBar <- physicalBar tc
    pBeat <- physicalBeat tc
    let changecps newtc = newtc { tempoClockName = "TempoClock"
                                , cps = c
                                , beatsPerCycle = beatsPerCycle tc
                                , startTime = now
                                , startBar = pBar
                                , startBeat = pBeat
                                , clockLatency = clockLatency tc
                                }
    modifyTempoClock db changecps

-- alias
cps' = modifyCps

---------------------------------------------------------------------
-- tick loop
---------------------------------------------------------------------

startTick :: DataBase -> IO ()
startTick db = do
    Just tc <- lookupM "TempoClock" $ tempoClockMMap db
    _ <- forkIO $ tickLoop db
    return ()

tickLoop :: DataBase -> IO ()
tickLoop db = do
    Just tc <- lookupM "TempoClock" $ tempoClockMMap db
    pb <- physicalBeat tc
    lb <- logicalBeatTime tc pb
    let lo = lb + clockLatency tc
    processPlayers db lo
    density <- getDensity db
    --putStrLn $ "density is " ++ show density
    sleep $ beatDelta tc
    tickLoop db

processPlayers :: DataBase -> Double -> IO ()
processPlayers db lo = do
    pnames <- getPlayerNames db
    let playPlayers = map (\p -> forkIO $ play db p lo) pnames
    sequence_ playPlayers

---------------------------------------------------------------------
-- Note density
---------------------------------------------------------------------

getDensity :: DataBase -> IO Double
getDensity db = do
    players <- getPlayers db
    let activePlayers = filter (\p -> playerStatus p == Playing) players
        activePlayPlayers = filter (\p -> playerStatus p == Playing) activePlayers
        playFlags = map (\p -> playFlag p) activePlayers
    return $ (sum playFlags) / fromIntegral (length playFlags)

---------------------------------------------------------------------
-- These functions are used to get values related to time 
-- physical means actual reality value
---------------------------------------------------------------------

currentTime :: IO Double
currentTime = do
    now <- getPOSIXTime
    return $ realToFrac now

-- physical time since tempo was changed
physicalTime :: TempoClock -> IO Double
physicalTime tc = do
    now <- currentTime
    let st = startTime tc
    return $ now - st

physicalBar :: TempoClock -> IO Double
physicalBar tc = do
    pt <- physicalTime tc
    let sb = startBar tc
        barDelta = fromIntegral $ ceiling $ pt * (cps tc)
    return $ sb + barDelta

physicalBeat :: TempoClock -> IO Double
physicalBeat tc = do
    pt <- physicalTime tc
    let sb = startBeat tc
        beatDelta = (cps tc) * (beatsPerCycle tc)
    return $ sb + beatDelta * pt

---------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------

-- sleep means threadDelay which receive Double argument
-- ex.) sleep 0.5 means threadDelay 0.5 secnds
sleep :: RealFrac a => a -> IO ()
sleep t = threadDelay $ truncate $ t * 1000000

-- physical time between beats
beatDelta :: TempoClock -> Double
beatDelta tc = (1 / cps tc) / beatsPerCycle tc

logicalBeatTime :: TempoClock -> Double -> IO Double
logicalBeatTime tc b = return $ st + beatDelta'
    where sb = startBeat tc
          st = startTime tc
          beatDelta' = (b - sb) * (beatDelta tc)
