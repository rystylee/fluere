module Sound.Pulse.Clock where

import Control.Concurrent (threadDelay, forkIO)

import Sound.Pulse.PulseMutableMap


data Clock = Clock { clockName :: String
                    ,clockBpm :: Int
                    ,clockStatus :: ClockStatus
                   } deriving (Show)

data ClockStatus =   Started
                   | Stopped deriving (Show, Eq)


-- Conversion between bpm and actual time
secondsToBpm :: Int -> Int
secondsToBpm seconds = 60 * ceiling (1 / fromIntegral seconds)

bpmToSeconds :: Int -> Int
bpmToSeconds bpm = ceiling (1 / (60 / fromIntegral bpm))


-- Used to Create a new Clock
newClock :: String -> Int -> ClockStatus -> IO Clock
newClock name bpm status = do
    let clock = Clock { clockName = name, clockBpm = bpm, clockStatus = status }
    return clock

--
newClockPulseMMap key clock = newPulseMMap [(key, clock)]

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

task :: IO ()
task = putStrLn "clock task"

loopClock :: IO Clock -> IO ()
loopClock clock = do
    clock' <- clock
    forkIO $ task
    threadDelay (clockBpm clock' * 100 * 1000)
