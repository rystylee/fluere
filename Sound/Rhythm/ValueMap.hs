module Sound.Rhythm.ValueMap where

import Control.Monad (replicateM)
import System.Random (getStdRandom, randomR)

import Sound.Fluere.Data
import Sound.Fluere.TempoClock (currentTime)
import Sound.Rhythm.Simplex (noise2D)


---------------------------------------------------------------------
-- NoiseMap
---------------------------------------------------------------------

newNoiseMap :: Int -> (Double, Double) -> IO ValueMap
newNoiseMap len base = do
    vs <- newValues len base
    return NoiseMap { mapLength = len, baseValues = base, values = vs }

newValues :: Int -> (Double, Double) -> IO [Double]
newValues len base = replicateM len (newNoiseValue base)

newNoiseValue :: (Double, Double) -> IO Double
newNoiseValue base = do
    now <- currentTime
    rand <- (getStdRandom $ randomR (0, 10) :: IO Double)
    let v = linearTransformation (noise2D now rand) (-1, 1) base
    return $ fromIntegral $ roundUpOn5 v

--newNoise :: (Double, Double) -> IO Double
--newNoise base = do
--    now <- currentTime
--    rand <- (getStdRandom $ randomR (0, 10) :: IO Double)
--    putStrLn $ show rand
--    return $ linearTransformation (noise2D now pi) (-1, 1) base

linearTransformation :: Double -> (Double, Double) -> (Double, Double) -> Double
linearTransformation v (minSource, maxSource) (minTarget, maxTarget) = val
    where val = (v - minSource) / (maxSource - minSource) * (maxTarget - minTarget) + minTarget

---------------------------------------------------------------------
-- Util
---------------------------------------------------------------------

roundUpOn5 :: (RealFrac a, Integral b) => a -> b
roundUpOn5 x
  | n <= -0.5 = m - 1
  | n >= 0.5 = m + 1
  | otherwise = m
  where (m, n) = properFraction x
