module Sound.Fluere.Core.Complexity where

import Data.Map.Strict as M

import Sound.Fluere.Core.Time (physicalBeat)
import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Debug
---------------------------------------------------------------------

showGlobalComplexityMap :: Environment -> String -> IO ()
showGlobalComplexityMap e n = do
    Just d <- lookupM n $ complexityMMap e
    putStrLn $ show $ complexityMap d

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newComplexity :: String -> Double -> ComplexityMap -> Complexity
newComplexity n cr cm = Complexity { complexityName = n
                                   , complexityRange = cr
                                   , complexityMap = cm
                                   }

newComplexityMMap :: Complexity -> IO (MutableMap String Complexity)
newComplexityMMap c = fromListM [(complexityName c, c)]

addComplexity :: Environment -> Complexity -> IO ()
addComplexity e c = insertM (complexityName c) c $ complexityMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapComplexity :: Environment -> String -> (Complexity -> Complexity) -> IO ()
swapComplexity e n f = do
    let cmmap = complexityMMap e
    Just c <- lookupM n cmmap
    let newc = f c
    insertM n newc cmmap

swapComplexityMap :: Environment -> String -> ComplexityMap -> IO ()
swapComplexityMap e n newcm = swapComplexity e n swapcm
    where cr = fromIntegral $ size newcm
          swapcm c = c { complexityRange = cr, complexityMap = newcm }

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------

currentComplexity :: Environment -> String -> IO Double
currentComplexity e n = do
    Just c <- lookupM n $ complexityMMap e
    let cmap = complexityMap c
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` floor (complexityRange c)
    let Just v = M.lookup val cmap
    --putStrLn $ show v
    return $ v


linearComplexityMap :: Double -> ComplexityMap
linearComplexityMap range = M.fromList $ zip [0,1..floor range] $ Prelude.map (\r -> scale r 0 range 0 1) [0,1..range]

sinComplexityMap :: Double -> ComplexityMap
sinComplexityMap range = M.fromList $ zip [0,1..floor range] $ Prelude.map (\r -> sinVal r range) [0,1..range]


currentDensity :: Environment -> String -> IO Double
currentDensity e n = do
    Just d <- lookupM n $ complexityMMap e
    let dmap = complexityMap d
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` floor (complexityRange d)
    let Just v = M.lookup val dmap
    --putStrLn $ show v
    return $ v


scale :: Double -> Double -> Double -> Double -> Double -> Double
scale v from to from' to' = (v - from) / (to - from) * (to' - from') + from'

sinVal :: Double -> Double -> Double
sinVal v range = sin (v * (pi / range))
