module Sound.Fluere.Core.Action where

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newAction :: Action -> Action
newAction (PlaySound n hi) = PlaySound { actionName = n, handleInstrument = hi }

newActionMMap :: Action -> IO (MutableMap String Action)
newActionMMap (PlaySound n hi) = fromListM [(n, (PlaySound n hi))]

addAction :: Environment -> Action -> IO ()
addAction e (PlaySound n hi) = insertM n (PlaySound n hi) $ actionMMap e
