module Sound.Fluere.Core.Time where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)

import Sound.Fluere.Core.BaseData


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
        beatDelta = (cps tc) * (bpc tc)
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
beatDelta tc = (1 / cps tc) / bpc tc

logicalTimeOfBeat :: TempoClock -> Double -> IO Double
logicalTimeOfBeat tc b = return $ st + beatDelta'
    where sb = startBeat tc
          st = startTime tc
          beatDelta' = (b - sb) * (beatDelta tc)
