module Sound.Fluere.Core.IOISet where

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, lookupM, insertM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newIOISet :: String -> Int -> [Double] -> Int -> IOISet
newIOISet n l pl c = IOISet { ioiSetName = n
                            , ioiSetLength = l
                            , ioiProbList = pl
                            , ioiCounter = c
                            }

newIOISetMMap :: IOISet -> IO (MutableMap String IOISet)
newIOISetMMap ioi = fromListM [(ioiSetName ioi, ioi)]

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapIOISet :: Environment -> String -> (IOISet -> IOISet) -> IO ()
swapIOISet e n f = do
    let ioimmap = ioiSetMMap e
    Just ioi <- lookupM n ioimmap
    let newioi = f ioi
    insertM n newioi ioimmap

swapIOIProbList :: Environment -> String -> [Double] -> IO ()
swapIOIProbList e n newpl = swapIOISet e n swappl
    where swappl ioi = ioi { ioiSetLength = length newpl
                           , ioiProbList = newpl
                           , ioiCounter = 0
                           }

swapIOICounter :: Environment -> String -> Int -> IO ()
swapIOICounter e n newc = swapIOISet e n swapc
    where swapc ioi = ioi { ioiCounter = newc }

---------------------------------------------------------------------
-- 
---------------------------------------------------------------------

nextProb :: Environment -> String -> IO Double
nextProb e n = do
    Just p <- lookupM n $ playerMMap e
    Just ioi <- lookupM (playerIOISet p) (ioiSetMMap e)
    let pl = ioiProbList ioi
        c = ioiCounter ioi
    if (c == (length pl - 1))
        then do
            swapIOICounter e (playerIOISet p) 0
        else do
            swapIOICounter e (playerIOISet p) (c + 1)
    return $ pl !! c
