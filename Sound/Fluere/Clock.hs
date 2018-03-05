module Sound.Fluere.Clock where

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Data.Time.Clock.POSIX


-- Used to create a new Clock
newClock :: String -> Double -> Double -> Double -> Double -> Double -> Double -> Clock
newClock cn c be st et ebar ebeat =
    Clock {  clockName = cn
            ,cps  = c
            ,beat = be
            ,startTime = st
            ,elapsedTime = et
            ,elapsedBar = ebar
            ,elapsedBeat = ebeat
          }

-- Used to create a new ClockMutableMap
newClockMMap :: Clock -> IO (TVar (Map String Clock))
newClockMMap clock =
    newMMap [(clockName clock, clock)]


-- The base function to change clock
changeClock :: DataBase -> String -> (Clock -> Clock) -> IO ()
changeClock db cname f = do
    let cmmap = clockMMap db
    Just clock <- findValueFromMMap cname cmmap
    let newClock = f clock
    addValToMMap (cname, newClock) cmmap

changeCps :: DataBase -> String -> Double -> IO ()
changeCps db cname newcps = do
    let changecps c = c { cps = newcps }
    changeClock db cname changecps

changeBeat :: DataBase -> String -> Double -> IO ()
changeBeat db cname newbeat = do
    let changebeat c = c { beat = newbeat }
    changeClock db cname changebeat


-- sleep means threadDelay which receive Double argument
sleep :: RealFrac a => a -> IO ()
sleep t = threadDelay ((truncate t * 100) * 10 * 1000)

-- Cast from POSIX Time to a Double
currentTime :: IO Double
currentTime = do
    n <- getPOSIXTime
    return $ realToFrac n

getElapsedTime :: Clock -> IO Double
getElapsedTime clock = do
    ct <- currentTime
    return $ (ct - startTime clock)

currentBar :: Clock -> IO Double
currentBar clock = do
    let cps' = cps clock
    et <- getElapsedTime clock
    return $ fromIntegral (floor (et * cps'))

currentBeat :: Clock -> IO Double
currentBeat clock = do
    et <- getElapsedTime clock
    let delta = beatToDelta clock
    return $ fromIntegral (floor (et / delta))


-- Convert beat to delta time
-- ex.) beat = 4, cps = 0.5 => delta = 0.5
beatToDelta :: Clock -> Double
beatToDelta clock =
    let cps' = cps clock
        beat' = beat clock
    in (1 / cps') / beat'

beatToTime :: Clock -> Double -> IO Double
beatToTime clock beat' = do
    let st = startTime clock
        delta = beatToDelta clock
    return $ st + (delta * beat')
