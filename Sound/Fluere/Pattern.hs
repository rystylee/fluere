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


-- Used to create a new Pattern
newPattern :: String -> [Double] -> Pattern
newPattern pname interval' = Pattern {
     patternName = pname
    ,interval = interval'
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

changeInterval :: DataBase -> String -> [Double] -> IO ()
changeInterval db pname newinterval = do
    let changeinterval p = p { interval = newinterval }
    changePattern db pname changeinterval

changeIndex :: DataBase -> String -> Int -> IO ()
changeIndex db pname newindex = do
    let changeindex p = p { index = newindex }
    changePattern db pname changeindex

------------------------------------------------------

nextBeat :: DataBase -> String -> Double -> IO Double
nextBeat db aname currentBeat' = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    Just clock <- findValueFromMMap (agentClock agent) (clockMMap db)
    Just pattern <- findValueFromMMap (agentPattern agent) (patternMMap db)
    let interval' = interval pattern
        index' = index pattern
        ilen = length interval'
    if (index' == (ilen - 1))
        then do
            changeIndex db (agentPattern agent) 0
            return $ (interval' !! index') * (currentDelta clock)
        else do
            changeIndex db (agentPattern agent) (index' + 1)
            return $ (interval' !! index') * (currentDelta clock)

------------------------------------------------------

--newSimilarPattern :: [[a]] -> Int -> [a]
--newSimilarPattern 2ls len = do
--    let ls = concat 2ls


-- Concat the list and add it to patternMMap
newConcatPattern :: DataBase -> String -> [[Double]] -> Double -> IO ()
newConcatPattern db pname ls beat' = do
    let newls = adjustList ls beat'
        newp = newPattern pname newls
    putStrLn (show newls)
    addPattern db newp



adjustList :: [[Double]] -> Double -> [Double]
adjustList ls beat' = do
    finalls <- forM ls $ \e -> do
        let ls' = adjustToBeat e beat'
        return ls'
    concat finalls

adjustToBeat :: [Double] -> Double -> [Double]
adjustToBeat ls beat'
    | beat' == (sum ls)    = ls
    | beat' - (sum ls) > 0 = increaseToBeat ls beat' (sum ls)
    | otherwise            = decreaseToBeat ls beat' (sum ls)

increaseToBeat :: [Double] -> Double -> Double -> [Double]
increaseToBeat ls beat' sum'
    | sum' == beat' = ls
    | otherwise     = ls ++ [beat' - sum']

decreaseToBeat :: [Double] -> Double -> Double -> [Double]
decreaseToBeat ls beat' sum'
    | sum' == beat' = ls
    | sum' < beat'  = increaseToBeat ls beat' sum'
    | sum' > beat'  = decreaseToBeat (init ls) beat' sum'

------------------------------------------------------

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
    in L.unfoldr phi


newScore :: Int -> [Int] -> [[Int]]
newScore abeat newscore =
    sliceWithBeat abeat newscore
