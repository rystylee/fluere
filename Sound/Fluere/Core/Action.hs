module Sound.Fluere.Core.Action where

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData
import Sound.Fluere.Core.SynthDef (swapFreq)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newAction :: Action -> Action
newAction (PlaySound n hsd) =
    PlaySound { actionName = n, handleSynthDef = hsd }
newAction (TuneFreq n hsd fs ac) =
    TuneFreq { actionName = n, handleSynthDef = hsd, freqs = fs, actionCounter = ac }
newAction (RepeatAction n ha ac) =
    RepeatAction { actionName = n, handleActions = ha, actionCounter = 0 }

newActionMMap :: Action -> IO (MutableMap String Action)
newActionMMap (PlaySound n hsd) = fromListM [(n, (PlaySound n hsd))]
newActionMMap (TuneFreq n hsd fs ac) = fromListM [(n, (TuneFreq n hsd fs ac))]
newActionMMap (RepeatAction n ha ac) = fromListM [(n, (RepeatAction n ha ac))]

addAction :: Environment -> Action -> IO ()
addAction e (PlaySound n hsd) = insertM n (PlaySound n hsd) $ actionMMap e
addAction e (TuneFreq n hsd fs ac) = insertM n (TuneFreq n hsd fs ac) $ actionMMap e
addAction e (RepeatAction n ha ac) = insertM n (RepeatAction n ha ac) $ actionMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapAction :: Environment -> String -> (Action -> Action) -> IO ()
swapAction e n f = do
    let ammap = actionMMap e
    Just a <- lookupM n ammap
    let newa = f a
    insertM n newa ammap

swapActionCounter :: Environment -> String -> Int -> IO ()
swapActionCounter e n newac = swapAction e n swapac
    where swapac a = a { actionCounter = newac }


---------------------------------------------------------------------


nextFreq :: Environment -> Action -> IO Double
nextFreq e a = do
    let fs = freqs a
        ac = actionCounter a
    if ac == (length fs - 1)
        then do
            swapActionCounter e (actionName a) 0
        else do
            swapActionCounter e (actionName a) (ac + 1)
    return $ fs !! ac

nextAction :: Environment -> Action -> IO Action
nextAction e a = do
    let ha = handleActions a
        ac = actionCounter a
    if ac == (length ha - 1)
        then do
            swapActionCounter e (actionName a) 0
        else do
            swapActionCounter e (actionName a) (ac + 1)
    Just na <- lookupM (actionName a) $ actionMMap e
    return na
