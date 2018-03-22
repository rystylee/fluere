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

-- Used to get nect beat of player
nextBeat :: DataBase -> String -> Double -> IO Double
nextBeat db pname currentBeat' = do
    Just player <- findValueFromMMap pname (playerMMap db)
    Just clock <- findValueFromMMap (playerClock player) (clockMMap db)
    Just pattern <- findValueFromMMap (playerPattern player) (patternMMap db)
    let rhythmList' = rhythmList pattern
        index' = index pattern
        ilen = length rhythmList'
    if (index' == (ilen - 1))
        then do
            changeIndex db (playerPattern player) 0
            return $ (rhythmList' !! index') * (currentDelta clock)
        else do
            changeIndex db (playerPattern player) (index' + 1)
            return $ (rhythmList' !! index') * (currentDelta clock)
