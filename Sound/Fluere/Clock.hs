module Sound.Fluere.Clock where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.BaseData
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )

-- Util
--
-- Conversion between bpm and actual time
secondsToBpm :: Double -> Int
secondsToBpm seconds = 60 * ceiling (1 / seconds)

bpmToSeconds :: Double -> Int
bpmToSeconds bpm = ceiling (1 / (60 / bpm))
--
--

-- These functions are used to create data with Clock
--
-- Used to create a new Clock
newClock :: String -> Double -> Double -> Double -> Clock
newClock name bpm beat starttime =
    Clock { clockName = name, clockBpm = bpm, clockBeat = beat, startTime = starttime }

-- Used to create a new ClockMutableMap
newClockMMap :: Clock -> IO (TVar (Map String Clock))
newClockMMap clock = newMMap [(clockName clock, clock)]
--
--

-- The base function to change clock
changeClock :: FluereWorld -> String -> (Clock -> Clock) -> IO ()
changeClock world cname f = do
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap cname cmmap
    let newClock = f clock
    addValToMMap (cname, newClock) cmmap

changeClockBpm :: FluereWorld -> String -> Double -> IO ()
changeClockBpm world cname newbpm = do
    let changebpm c = c { clockBpm = newbpm }
    changeClock world cname changebpm

changeClockBeat :: FluereWorld -> String -> Double -> IO ()
changeClockBeat world cname newbeat = do
    let changebeat c = c { clockBeat = newbeat }
    changeClock world cname changebeat

--
--task :: IO ()
--task = putStrLn "clock task"
--
--loopClock :: Clock -> IO ()
--loopClock clock = do
--    forkIO task
--    threadDelay (clockBpm clock * 100 * 1000)
