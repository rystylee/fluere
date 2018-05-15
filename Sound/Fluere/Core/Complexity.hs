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

newComplexity :: String -> Int -> (Double, Double) -> ComplexityMap -> Complexity
newComplexity n l cr cm = Complexity { complexityName = n
                                     , complexityMapLength = l
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
    where swapcm c = c { complexityMapLength = size newcm, complexityMap = newcm }

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------

currentComplexity :: Environment -> String -> IO Double
currentComplexity e n = do
    Just c <- lookupM n $ complexityMMap e
    Just tc <- lookupM "TempoClock" $ tempoClockMMap e
    pb <- physicalBeat tc
    let val = (floor pb) `mod` complexityMapLength c
    let Just v = M.lookup val $ complexityMap c
    --putStrLn $ show v
    return $ v


linearComplexityMap :: Int -> (Double, Double) -> ComplexityMap
linearComplexityMap l (min', max') = M.fromList $ zip [0,1..l] $ Prelude.map (\v -> scale v 0 (fromIntegral l) min' max') [0,1..fromIntegral l]

sinComplexityMap :: Int -> (Double, Double) -> ComplexityMap
sinComplexityMap l (min', max') = M.fromList $ zip [0,1..l] $ Prelude.map (\r -> sinVal r (fromIntegral l)) [0,1..fromIntegral l]


-- Utils
scale :: Double -> Double -> Double -> Double -> Double -> Double
scale v from to from' to' = (v - from) / (to - from) * (to' - from') + from'

sinVal :: Double -> Double -> Double
sinVal v range = sin (v * (pi / range))
