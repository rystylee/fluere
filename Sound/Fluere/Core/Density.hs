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

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newDensity :: String -> Double -> DensityMap -> Density
newDensity n dr dm = Density { densityName = n
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
    where dr = fromIntegral $ size newdm
          swapdm d = d { densityRange = dr, densityMap = newdm }

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------

linearDensityMap :: Double -> DensityMap
linearDensityMap range = M.fromList $ zip [0,1..floor range] $ Prelude.map (\r -> scale r 0 range 0 1) [0,1..range]

sinDensityMap :: Double -> DensityMap
sinDensityMap range = M.fromList $ zip [0,1..floor range] $ Prelude.map (\r -> sinVal r range) [0,1..range]


currentDensity :: Environment -> String -> IO Double
currentDensity e n = do
    Just d <- lookupM n $ densityMMap e
    let dmap = densityMap d
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` floor (densityRange d)
    let Just v = M.lookup val dmap
    --putStrLn $ show v
    return $ v


scale :: Double -> Double -> Double -> Double -> Double -> Double
scale v from to from' to' = (v - from) / (to - from) * (to' - from') + from'

sinVal :: Double -> Double -> Double
sinVal v range = sin (v * (pi / range))
