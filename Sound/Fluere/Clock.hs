module Sound.Fluere.Clock where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Data.Map (Map)
import Data.Time.Clock.POSIX

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )


------------------------------------------------------
-- For debug
------------------------------------------------------
 

displayClock :: DataBase -> String -> IO ()
displayClock db cname = do
    Just clock <- findValueFromMMap cname (clockMMap db)
    et <- elapsedTime clock
    cBar <- currentBar clock
    cBeat <- currentBeat clock
    let sBar = startBar $ currentTempoHistory clock
    let sBeat = startBeat $ currentTempoHistory clock
    putStrLn $ "\n------------------------------------" 
    putStrLn $ "clockName : " ++ show (clockName clock)
    putStrLn $ "elapsedTime : " ++ show et
    putStrLn $ "currentBar : " ++ show cBar
    putStrLn $ "currentBeat : " ++ show cBeat
    putStrLn $ "------------------------------------\n" 
    sleep $ 5
    void (forkIO $ displayClock db cname)


------------------------------------------------------

-- Used to create a new Clock
newClock :: String -> [TempoHistory] -> Clock
newClock cname' tempohistories' =
    Clock { clockName = cname'
           ,tempoHistories = tempohistories'
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

changeTempoHistories :: DataBase -> String -> TempoHistory -> IO ()
changeTempoHistories db cname newtempohistory = do
    Just clock <- findValueFromMMap cname (clockMMap db)
    let changetempohistories c = c { tempoHistories = newtempohistory:(tempoHistories clock) }
    changeClock db cname changetempohistories
    displayClock db cname

changeTempo :: DataBase -> String -> Double -> Double -> IO ()
changeTempo db cname cps' beat' = do
    Just clock <- findValueFromMMap cname (clockMMap db)
    cTime <- currentTime
    cBar <- currentBar clock
    cBeat <- currentBeat clock
    let b = beat $ tempo $ currentTempoHistory clock
        sBar = startBar $ currentTempoHistory clock
        sBeat = startBeat $ currentTempoHistory clock
        nBar = cBar + 1
        nBeat = sBeat + (nBar - sBar) * b
        newtempo = Tempo { cps = cps', beat = beat' }
        newtempohistory = TempoHistory { tempo = newtempo
                                        ,startTime = cTime
                                        ,startBar = nBar
                                        ,startBeat = nBeat
                                        ,lastBar = cBar
                                        ,lastBeat = cBeat
                                       }
    changeTempoHistories db cname newtempohistory


-- sleep means threadDelay which receive Double argument
sleep :: RealFrac a => a -> IO ()
sleep t = threadDelay ((truncate t * 100) * 10 * 1000)

-- Get delta of each beat by cps and beat
-- ex.) cps = 0.5, beat = 4 => delta = 0.5
beatToDelta :: Double -> Double -> Double
beatToDelta cps' beat' = (1 / cps') / beat'

-- Get time by beat
-- ex.) cps = 0.5, beat = 4 , beat (arg) = 20 => time = 10
beatToTime :: Clock -> Double -> Double
beatToTime clock beat' =
    let cps' = cps $ currentTempo clock
        delta' = beatToDelta cps' (beat $ currentTempo clock)
    in delta' * beat'

-- Get current delta, which is the time between currentBeat and NextBeat
-- ex.) beat = 4, cps = 0.5 => delta = 0.5
currentDelta :: Clock -> Double
currentDelta clock =
    let tempo' = currentTempo clock
        cps' = cps tempo'
        beat' = beat tempo'
    in (1 / cps') / beat'

-- Cast from POSIX Time to a Double
currentTime :: IO Double
currentTime = do
    n <- getPOSIXTime
    return $ realToFrac n

-- Get the elapsed time since Tempo was changed
elapsedTime :: Clock -> IO Double
elapsedTime clock = do
    ct <- currentTime
    let st = startTime (currentTempoHistory clock)
    return $ (ct - st)

-- Helper to get current TempoHistory
currentTempoHistory :: Clock -> TempoHistory
currentTempoHistory clock = head (tempoHistories clock)

-- Helper to get current Tempo
currentTempo :: Clock -> Tempo
currentTempo clock = tempo (currentTempoHistory clock)

-- Get the current Bar since Tempo was changed
currentBar :: Clock -> IO Double
currentBar clock = do
    et <- elapsedTime clock
    let sBar = lastBar (currentTempoHistory clock)
    let cps' = cps $ tempo (currentTempoHistory clock)
    return $ sBar + fromIntegral (floor (et * cps'))

-- Get the current Beat since Tempo was changed
currentBeat :: Clock -> IO Double
currentBeat clock = do
    et <- elapsedTime clock
    let sBeat = lastBeat (currentTempoHistory clock)
    let delta = currentDelta clock
    return $ sBeat + et / delta