module Sound.Fluere.Core.Action where

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newAction :: Action -> Action
newAction (PlaySound n hsd) = PlaySound { actionName = n, handleSynthDef = hsd }

newActionMMap :: Action -> IO (MutableMap String Action)
newActionMMap (PlaySound n hsd) = fromListM [(n, (PlaySound n hsd))]

addAction :: Environment -> Action -> IO ()
addAction e (PlaySound n hsd) = insertM n (PlaySound n hsd) $ actionMMap e
