module Sound.Fluere.Clock where

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.BaseData
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Time (currentTime, beatToDeltaByBpm, sleep)


-- These functions are used to create data with Clock
--
-- Used to create a new Clock
newClock :: String -> Double -> Double -> Double -> Double -> Double -> Clock
newClock name bpm beat lasteventime nexteventtime starttime =
    Clock {  clockName = name
            ,clockBpm = bpm
            ,clockBeat = beat
            ,lastEventTime = lasteventime
            ,nextEventTime = nexteventtime
            ,startTime = starttime
          }

-- Used to create a new ClockMutableMap
newClockMMap :: Clock -> IO (TVar (Map String Clock))
newClockMMap clock =
    newMMap [(clockName clock, clock)]
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

changeLastEventTime :: FluereWorld -> String -> Double -> IO ()
changeLastEventTime world cname newlasteventtime = do
    let changelasteventtime c = c { lastEventTime = newlasteventtime }
    changeClock world cname changelasteventtime

changeNextEventTime :: FluereWorld -> String -> Double -> IO ()
changeNextEventTime world cname newnexteventtime = do
    let changenexteventtime c = c { nextEventTime = newnexteventtime }
    changeClock world cname changenexteventtime


-- Used to measure the time
--
startClock :: FluereWorld -> String -> IO ThreadId
startClock world cname = do
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap cname cmmap
    let bpm = clockBpm clock
        beat = clockBeat clock
        delta = beatToDeltaByBpm bpm beat
    ct <- currentTime
    changeLastEventTime world cname (ct)
    changeNextEventTime world cname (ct + delta + 1.0)
    --loopClock world cname
    forkIO $ loopClock world cname

stopClock :: ThreadId -> IO ()
stopClock id = killThread id

loopClock :: FluereWorld -> String -> IO ()
loopClock world cname = do
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap cname cmmap
    let bpm = clockBpm clock
        beat = clockBeat clock
        delta = beatToDeltaByBpm bpm beat
    ct <- currentTime
    let nt = nextEventTime clock
    if (nt > ct)
        then do
            let diff = nt - ct
            sleep diff
            loopClock world cname
        else do
            ct' <- currentTime
            changeLastEventTime world cname ct'
            changeNextEventTime world cname (ct' + delta)
            --putStrLn $ "tick tuck" ++ (show ct')
            loopClock world cname
