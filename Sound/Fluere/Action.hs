module Sound.Fluere.Action ( newAction
                           , newActionMMap
                           , addAction
                           , act
                           ) where

import Sound.OSC.FD (Datum)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM)
import Sound.Fluere.Osc (sendToSC)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newAction :: Action -> Action
newAction (PlaySound n om) = PlaySound { actionName = n, oscMessage = om }
newAction (DisplayMessage n dm) = DisplayMessage { actionName = n, displayMessage = dm }

newActionMMap :: Action -> IO (MutableMap String Action)
newActionMMap (PlaySound n om) = fromListM [(n, (PlaySound n om))]
newActionMMap (DisplayMessage n dm) = fromListM [(n, (DisplayMessage n dm))]

addAction :: DataBase -> Action -> IO ()
addAction db (PlaySound n om) = insertM n (PlaySound n om) $ actionMMap db
addAction db (DisplayMessage n dm) = insertM n (DisplayMessage n dm) $ actionMMap db

---------------------------------------------------------------------
-- Different action function for each player
---------------------------------------------------------------------

act :: DataBase -> Player -> Action -> Double -> IO ()
act db p (PlaySound _ om) lo = sendToSC lo om
act db p (DisplayMessage n dm) lo = putStrLn $ show n ++ "'s message is " ++ show dm
