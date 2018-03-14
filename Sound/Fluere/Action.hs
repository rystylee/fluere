module Sound.Fluere.Action where

import Control.Concurrent.STM (TVar)
import Data.Map

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.OSC (sendToSC)


-- Used to create a new Action
newAction :: String -> (DataBase -> String -> IO ()) -> Action
newAction aname actfunc =
    Action { actionName = aname
            ,actionFunc = actfunc
           }

-- Used to create a new Action MutableMap
newActionMMap :: Action -> IO (TVar (Map String Action))
newActionMMap act = newMMap [(actionName act, act)]

-- Used to add a new Action to MutableMap
addNewAction :: DataBase -> Action -> IO ()
addNewAction db act = do
    addValToMMap (actionName act, act) (actionMMap db)

------------------------------------------------------
-- An action used to play sound
------------------------------------------------------

playSound :: DataBase -> String -> IO ()
playSound db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    sendToSC "s_new" (agentOscMessage agent)

------------------------------------------------------
