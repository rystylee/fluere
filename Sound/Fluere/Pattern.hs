module Sound.Fluere.Pattern where

import Control.Concurrent.STM (TVar)
import Control.Monad (void, forM)
import qualified Data.Map as M
import qualified Data.List as L

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Clock (currentDelta)


---------------------------------------------------------------------

-- Used to create a new Pattern
newPattern :: String -> [Double] -> Pattern
newPattern pname rhythmList' =
    Pattern { patternName = pname
             ,rhythmList = rhythmList'
             ,index = 0
            }

-- Used to create a new Pattern MutableMap
newPatternMMap :: Pattern -> IO (TVar (M.Map String Pattern))
newPatternMMap pattern = newMMap [(patternName pattern, pattern)]

-- Used to add a new Pattern to MutableMap
addPattern :: DataBase -> Pattern -> IO ()
addPattern db pattern = do
    addValToMMap (patternName pattern, pattern) (patternMMap db)

-- The base function to change Pattern
changePattern :: DataBase -> String -> (Pattern -> Pattern) -> IO ()
changePattern db pname f = do
    let pmmap = patternMMap db
    Just pattern <- findValueFromMMap pname pmmap
    let newPattern = f pattern
    addValToMMap (pname, newPattern) pmmap

changeRhythmList :: DataBase -> String -> [Double] -> IO ()
changeRhythmList db pname newrhythmlist = do
    let changerhythmlist p = p { rhythmList = newrhythmlist }
    changePattern db pname changerhythmlist

changeIndex :: DataBase -> String -> Int -> IO ()
changeIndex db pname newindex = do
    let changeindex p = p { index = newindex }
    changePattern db pname changeindex

---------------------------------------------------------------------

nextBeat :: DataBase -> String -> Double -> IO Double
nextBeat db aname currentBeat' = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    Just clock <- findValueFromMMap (agentClock agent) (clockMMap db)
    Just pattern <- findValueFromMMap (agentPattern agent) (patternMMap db)
    let rhythmList' = rhythmList pattern
        index' = index pattern
        ilen = length rhythmList'
    if (index' == (ilen - 1))
        then do
            changeIndex db (agentPattern agent) 0
            return $ (rhythmList' !! index') * (currentDelta clock)
        else do
            changeIndex db (agentPattern agent) (index' + 1)
            return $ (rhythmList' !! index') * (currentDelta clock)

---------------------------------------------------------------------
-- These function are used to create new patterns
---------------------------------------------------------------------

-- Replicate the list and add it to patternMMap
newReplPattern :: DataBase -> String -> [[Double]] -> Double -> IO ()
newReplPattern db pname ls beat' = do
    let newls = adjustList ls beat'
        newp = newPattern pname (concat newls)
    putStrLn (show newls)
    addPattern db newp

{--
beat = 4
ls1 = [[1,2,1], [2,2], [1.5,2.5]]
ls2 = [[2,2], [0.5,1.5,2], [1,3]]
--}

-- Used to create the pattern which is mixied two lists given as argument
newMixedPattern :: DataBase -> String -> [[Double]] -> [[Double]] -> Double -> IO ()
newMixedPattern db pname ls1 ls2 beat' = undefined



---------------------------------------------------------------------
-- For manipulating list utils
---------------------------------------------------------------------

adjustList :: [[Double]] -> Double -> [[Double]]
adjustList ls beat' = do
    finalls <- forM ls $ \e -> do
        let ls' = adjustToBeat e beat'
        return ls'
    finalls

adjustToBeat :: [Double] -> Double -> [Double]
adjustToBeat ls beat'
    | beat' == (sum ls)    = ls
    | beat' - (sum ls) > 0 = increaseToBeat ls beat' (sum ls)
    | beat' - (sum ls) < 0 = decreaseToBeat ls beat' (sum ls)

increaseToBeat :: [Double] -> Double -> Double -> [Double]
increaseToBeat ls beat' sum'
    | sum' == beat' = ls
    | sum' < beat'  = ls ++ [beat' - sum']
    | sum' > beat'  = decreaseToBeat ls beat' sum'

decreaseToBeat :: [Double] -> Double -> Double -> [Double]
decreaseToBeat ls beat' sum'
    | sum' == beat' = ls
    | sum' < beat'  = increaseToBeat ls beat' sum'
    | sum' > beat'  = decreaseToBeat (init ls) beat' (sum' - last ls)

---------------------------------------------------------------------
-- Used to calc the similarilty of two list
---------------------------------------------------------------------

-- If given the pair of list, then return the value (0 ~ 1)
calcSimilarity :: [[Double]] -> [[Double]] -> Double
calcSimilarity ls1 ls2 = undefined

