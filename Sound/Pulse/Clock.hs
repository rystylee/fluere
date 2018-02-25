module Sound.Pulse.Clock where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Pulse.PulseData
import Sound.Pulse.PulseMutableMap (newPulseMMap)


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

-- Used to create a new ClockPulseMutableMap
newClockPulseMMap :: Clock -> IO (TVar (Map String Clock))
newClockPulseMMap clock = newPulseMMap [(clockName clock, clock)]
--
--

-- These functions are used to change Clock
--
-- Used to change a Clock
--changeClock :: ClockStatus -> Int -> IO Clock
--changeClock s b = do
--    let clock = Clock { clockStatus = s, clockBpm = b}
--    return clock

-- Used to start a Clock
startClock :: IO Clock -> IO ()
startClock clock = do
    clock' <- clock
    if clockStatus clock' == Stopped
        then putStrLn "Clock starts."
        else putStrLn "Clock has been started."
 
-- Used to stop a Clock
stopClock :: IO Clock -> IO ()
stopClock clock = do
    clock' <- clock
    if clockStatus clock' == Started
        then putStrLn "Clock starts."
        else putStrLn "Clock has been stopped."
--
--

task :: IO ()
task = putStrLn "clock task"

loopClock :: Clock -> IO ()
loopClock clock = do
    forkIO task
    threadDelay (clockBpm clock * 100 * 1000)
