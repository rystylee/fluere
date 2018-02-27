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
secondsToBpm :: Int -> Int
secondsToBpm seconds = 60 * ceiling (1 / fromIntegral seconds)

bpmToSeconds :: Int -> Int
bpmToSeconds bpm = ceiling (1 / (60 / fromIntegral bpm))
--
--

-- These functions are used to create data with Clock
--
-- Used to create a new Clock
newClock :: String -> Int -> ClockStatus -> Clock
newClock name bpm status =
    Clock { clockName = name, clockBpm = bpm, clockStatus = status }

-- Used to create a new ClockMutableMap
newClockMMap :: Clock -> IO (TVar (Map String Clock))
newClockMMap clock = newMMap [(clockName clock, clock)]
--
--

-- These functions are not used during the performance
changeClock :: FluereWorld -> String -> (Clock -> Clock) -> IO ()
changeClock world cname f = do
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap cname cmmap
    let newClock = f clock
    addValToMMap (cname, newClock) cmmap
--
--

-- These functions are used during the performance
--
changeClockBpm :: FluereWorld -> String -> Int -> IO ()
changeClockBpm world cname newbpm = do
    let changebpm c = c { clockBpm = newbpm }
    changeClock world cname changebpm
--
--

task :: IO ()
task = putStrLn "clock task"

loopClock :: Clock -> IO ()
loopClock clock = do
    forkIO task
    threadDelay (clockBpm clock * 100 * 1000)
