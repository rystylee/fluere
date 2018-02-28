module Sound.Fluere.Time where

import Data.Time.Clock.POSIX


-- Cast from POSIX Time to a Double
currentTime :: IO Double
currentTime = do
    n <- getPOSIXTime
    return $ realToFrac n

-- Convert beat to delta time, by bpm
-- ex.) beat = 4, bpm = 60 => delta = 0.25
beatToDelta :: Double -> Double -> Double
beatToDelta beat bpm = (1 / (bpm / 60)) / beat
