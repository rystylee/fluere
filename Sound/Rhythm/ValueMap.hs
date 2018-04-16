module Sound.Rhythm.ValueMap ( newNoiseMap
                             , newLMap
                             , newRegularMap
                             ) where

import Control.Monad (replicateM)
import System.Random (getStdRandom, randomR)

import Sound.Fluere.Data
import Sound.Fluere.TempoClock (currentTime)
import Sound.Rhythm.Simplex (noise2D)
import Sound.Rhythm.LSystem (generateTree)


---------------------------------------------------------------------
-- RegularMap
---------------------------------------------------------------------

newRegularMap :: [[Double]] -> IO ValueMap
newRegularMap xs = do
    let xs' = newRegularValue xs
    return $ RegularMap { mapLength = length xs', values = xs' }

newRegularValue :: [[Double]] -> [Double]
newRegularValue xs = concat xs

---------------------------------------------------------------------
-- CycleMap
---------------------------------------------------------------------

newCycleMap = undefined

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

newLMap :: Int -> ([Double], [Double]) -> String -> String -> Int -> IO ValueMap
newLMap len symbol rule initial n = do
    let tree = newTree len symbol rule initial n
    return LMap { mapLength = len, symbolValues = symbol, values = tree }

newTree :: Int -> ([Double], [Double]) -> String -> String -> Int -> [Double]
newTree len symbol rule initial n = take len $ generateTree symbol rule initial n

---------------------------------------------------------------------
-- Util
---------------------------------------------------------------------

-- Map the value
linearTransformation :: Double -> (Double, Double) -> (Double, Double) -> Double
linearTransformation v (minSource, maxSource) (minTarget, maxTarget) = val
    where val = (v - minSource) / (maxSource - minSource) * (maxTarget - minTarget) + minTarget

roundUpOn5 :: (RealFrac a, Integral b) => a -> b
roundUpOn5 x
    | n <= -0.5 = m - 1
    | n >= 0.5 = m + 1
    | otherwise = m
    where (m, n) = properFraction x
