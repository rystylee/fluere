module Sound.Fluere.Core.Density where

import Data.Map.Strict as M

import Sound.Fluere.Core.Time (physicalBeat)
import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Debug
---------------------------------------------------------------------

showGlobalDensityMap :: Environment -> String -> IO ()
showGlobalDensityMap e n = do
    Just d <- lookupM n $ densityMMap e
    putStrLn $ show $ densityMap d

showCurrentDensity :: Environment -> String -> IO ()
showCurrentDensity e n = do
    Just d <- lookupM n $ densityMMap e
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` densityMapLength d
    let Just v = M.lookup val $ densityMap d
    putStrLn $ show v

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newDensity :: String -> Int -> (Double, Double) -> DensityMap -> Density
newDensity n l dr dm = Density { densityName = n
                               , densityMapLength = l
                               , densityRange = dr
                               , densityMap = dm
                               }

newDensityMMap :: Density -> IO (MutableMap String Density)
newDensityMMap d = fromListM [(densityName d, d)]

addDensity :: Environment -> Density -> IO ()
addDensity e d = insertM (densityName d) d $ densityMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapDensity :: Environment -> String -> (Density -> Density) -> IO ()
swapDensity e n f = do
    let dmmap = densityMMap e
    Just d <- lookupM n dmmap
    let newd = f d
    insertM n newd dmmap

swapDensityMap :: Environment -> String -> DensityMap -> IO ()
swapDensityMap e n newdm = swapDensity e n swapdm
    where swapdm d = d { densityMapLength = size newdm, densityMap = newdm }

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------

currentDensity :: Environment -> String -> IO Double
currentDensity e n = do
    Just d <- lookupM n $ densityMMap e
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` densityMapLength d
    let Just v = M.lookup val $ densityMap d
    --putStrLn $ show v
    return $ v


--linearDensityMap :: Double -> DensityMap
--linearDensityMap range = M.fromList $ zip [0,1..floor range] $ Prelude.map (\r -> scale r 0 range 0 1) [0,1..range]

linearDensityMap :: Int -> (Double, Double) -> DensityMap
linearDensityMap l (min', max') = M.fromList $ zip [0,1..l] $ Prelude.map (\v -> scale v 0 (fromIntegral l) min' max') [0,1..fromIntegral l]

sinDensityMap :: Int -> (Double, Double) -> DensityMap
sinDensityMap l (min', max') = M.fromList $ zip [0,1..l] $ Prelude.map (\r -> sinVal r (fromIntegral l)) [0,1..fromIntegral l]


-- Utils
scale :: Double -> Double -> Double -> Double -> Double -> Double
scale v from to from' to' = (v - from) / (to - from) * (to' - from') + from'

sinVal :: Double -> Double -> Double
sinVal v l = sin (v * (pi / l))
