module Sound.Rhythm.ValueMap where

import Control.Monad (replicateM)
import System.Random (getStdRandom, randomR)

import Sound.Fluere.Data
import Sound.Fluere.TempoClock (currentTime)
import Sound.Rhythm.Simplex (noise2D)
import Sound.Rhythm.LSystem (generateTree)


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

---------------------------------------------------------------------
-- LMap
---------------------------------------------------------------------

newLMap :: ([Double], [Double]) -> String -> String -> Int -> Int -> IO ValueMap
newLMap symbol rule initial n len = do
    let tree = newTree symbol rule initial n len
    return LMap { mapLength = len, symbolValues = symbol, values = tree }

newTree :: ([Double], [Double]) -> String -> String -> Int -> Int -> [Double]
newTree symbol rule initial n len = take len $ generateTree symbol rule initial n

---------------------------------------------------------------------
-- Util
---------------------------------------------------------------------

linearTransformation :: Double -> (Double, Double) -> (Double, Double) -> Double
linearTransformation v (minSource, maxSource) (minTarget, maxTarget) = val
    where val = (v - minSource) / (maxSource - minSource) * (maxTarget - minTarget) + minTarget

roundUpOn5 :: (RealFrac a, Integral b) => a -> b
roundUpOn5 x
  | n <= -0.5 = m - 1
  | n >= 0.5 = m + 1
  | otherwise = m
  where (m, n) = properFraction x
