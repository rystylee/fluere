module Sound.Fluere.Pattern where

import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Data.Map
import Data.List

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Clock (currentDelta)


-- Used to create a new Pattern
newPattern :: String -> [Double] -> Int -> Pattern
newPattern pname interval' counter' =
    Pattern { patternName = pname
             ,interval = interval'
             ,counter = counter'
            }

-- Used to create a new Pattern MutableMap
newPatternMMap :: Pattern -> IO (TVar (Map String Pattern))
newPatternMMap pattern = newMMap [(patternName pattern, pattern)]

-- Used to add a new Pattern to MutableMap
addNewPattern :: DataBase -> Pattern -> IO ()
addNewPattern db pattern = do
    addValToMMap (patternName pattern, pattern) (patternMMap db)

-- The base function to change Pattern
changePattern :: DataBase -> String -> (Pattern -> Pattern) -> IO ()
changePattern db pname f = do
    let pmmap = patternMMap db
    Just pattern <- findValueFromMMap pname pmmap
    let newPattern = f pattern
    addValToMMap (pname, newPattern) pmmap

changeInterval :: DataBase -> String -> [Double] -> IO ()
changeInterval db pname newinterval = do
    let changeinterval p = p { interval = newinterval }
    changePattern db pname changeinterval

changeCounter :: DataBase -> String -> Int -> IO ()
changeCounter db pname newcounter = do
    let changecounter p = p { counter = newcounter }
    changePattern db pname changecounter

------------------------------------------------------

nextBeat :: DataBase -> String -> Double -> IO Double
nextBeat db aname currentBeat' = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    Just clock <- findValueFromMMap (agentClock agent) (clockMMap db)
    Just pattern <- findValueFromMMap (agentPattern agent) (patternMMap db)
    let interval' = interval pattern
        counter' = counter pattern
        ilen = length interval'
    if (counter' == (ilen - 1))
        then do
            changeCounter db (agentPattern agent) 0
            return $ (interval' !! counter') * (currentDelta clock)
        else do
            changeCounter db (agentPattern agent) (counter' + 1)
            return $ (interval' !! counter') * (currentDelta clock)


fillRestWith0 :: Num a => Int -> [a] -> Maybe ([a], [a])
fillRestWith0 n xs =
    if length xs == n
        then Just $ splitAt n xs
        else do
            let (xs', xs_) = splitAt n xs
            Just (xs' ++ replicate (n - length xs) 0, xs_)

sliceWithBeat :: Num a => Int -> [a] -> [[a]]
sliceWithBeat n =
    let phi [] = Nothing
        phi xs = fillRestWith0 n xs
    in unfoldr phi


newScore :: Int -> [Int] -> [[Int]]
newScore abeat newscore =
    sliceWithBeat abeat newscore
