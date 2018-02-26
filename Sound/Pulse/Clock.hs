module Sound.Pulse.Clock where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Pulse.PulseData
import Sound.Pulse.PulseMutableMap ( newPulseMMap
                                    ,findValueFromPulseMMap
                                    ,addValToPulseMMap
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

-- Used to create a new ClockPulseMutableMap
newClockPulseMMap :: Clock -> IO (TVar (Map String Clock))
newClockPulseMMap clock = newPulseMMap [(clockName clock, clock)]
--
--

-- These functions are not used during the performance
changeClock :: PulseWorld -> String -> (Clock -> Clock) -> IO ()
changeClock world cname f = do
    let cmmap = wClockPulseMMap world
    Just clock <- findValueFromPulseMMap cname cmmap
    let newClock = f clock
    addValToPulseMMap (cname, newClock) cmmap

changeClockBpm :: PulseWorld -> String -> Int -> IO ()
changeClockBpm world cname newbpm = do
    let changebpm c = c { clockBpm = newbpm }
    changeClock world cname changebpm
--
--

-- These functions are used during the performance
--
--

task :: IO ()
task = putStrLn "clock task"

loopClock :: Clock -> IO ()
loopClock clock = do
    forkIO task
    threadDelay (clockBpm clock * 100 * 1000)
