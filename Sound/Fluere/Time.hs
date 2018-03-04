module Sound.Fluere.Time where

import Data.Time.Clock.POSIX
import Control.Concurrent (forkIO, threadDelay)


-- Cast from POSIX Time to a Double
currentTime :: IO Double
currentTime = do
    n <- getPOSIXTime
    return $ realToFrac n

-- sleep means threadDelay which receive Double argument
sleep :: RealFrac a => a -> IO ()
sleep t = threadDelay ((truncate t * 100) * 10 * 1000)

-- Convert beat to delta time, by bpm
-- ex.) beat = 4, bpm = 60 => delta = 0.25
beatToDeltaByBpm :: Double -> Double -> Double
beatToDeltaByBpm bpm beat = (1 / (bpm / 60)) / beat
