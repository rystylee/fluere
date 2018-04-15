module Sound.Fluere.Pattern ( newPattern
                            , newPatternMMap
                            , addPattern
                            , nextPlayerNote
                            , modifyValueMap
                            ) where

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM, lookupM)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newPattern :: String -> ValueMap -> Pattern
newPattern n vm = Pattern { patternName = n, valueMap = vm, index = 0 }

newPatternMMap :: Pattern -> IO (MutableMap String Pattern)
newPatternMMap pattern = fromListM [(patternName pattern, pattern)]

addPattern :: DataBase -> Pattern -> IO ()
addPattern db pattern = insertM (patternName pattern) pattern $ patternMMap db

---------------------------------------------------------------------
-- Modify
---------------------------------------------------------------------

modifyPattern :: DataBase -> String -> (Pattern -> Pattern) -> IO ()
modifyPattern db n f = do
    let pmmap = patternMMap db
    Just p <- lookupM n pmmap
    let newp = f p
    insertM n newp pmmap

modifyValueMap :: DataBase -> String -> ValueMap -> IO ()
modifyValueMap db n newvm = do
    let modifyvm p = p { valueMap = newvm, index = 0 }
    modifyPattern db n modifyvm
    putStrLn $ show newvm

modifyIndex :: DataBase -> String -> Int -> IO ()
modifyIndex db n newi = modifyPattern db n modifyi
    where modifyi p = p { index = newi }

---------------------------------------------------------------------
-- used to get player's next note
---------------------------------------------------------------------

nextPlayerNote :: DataBase -> String -> IO Double
nextPlayerNote db n = do
    Just p <- lookupM n $ playerMMap db
    Just pattern <- lookupM (playerPattern p) (patternMMap db)
    let ds = convertN $ values $ valueMap pattern
        i = index pattern
    if (i == (length ds - 1))
        then do
            modifyIndex db (playerPattern p) 0
        else do
            modifyIndex db (playerPattern p) (i + 1)
    return $ ds !! i

-- ex.) [2,1,3] -> [1,0,1,1,0,0]
convertN :: [Double] -> [Double]
convertN xs = concat $ map convert xs

convert :: Double -> [Double]
convert 0 = [0]
convert x = [1] ++ replicate ((floor x) - 1) 0
