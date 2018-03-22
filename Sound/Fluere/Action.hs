module Sound.Fluere.Action where

import Control.Concurrent.STM (TVar)
import Data.Map

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.OSC (sendToSC)
import Sound.Fluere.Agent (changeAgentStatus)


---------------------------------------------------------------------

-- Used to create a new Action
newAction :: String -> (String -> DataBase -> String -> IO ()) -> Action
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

---------------------------------------------------------------------
-- Different action function for each Agent
---------------------------------------------------------------------

act :: String -> DataBase -> String -> IO ()
act agentAction' db aname
    | agentAction' == "playSound" = do
        Just agent <- findValueFromMMap aname (agentMMap db)
        sendToSC "s_new" (agentOscMessage agent)

    | agentAction' == "swapAgentStatus" = do
        Just agent <- findValueFromMMap aname (agentMMap db)
        if (agentStatus agent == Playing)
            then changeAgentStatus db aname Pausing
            else changeAgentStatus db aname Playing

    | agentAction' == "putStrLn"          = putStrLn "Hello, World!"

    | otherwise = do
        Just agent <- findValueFromMMap aname (agentMMap db)
        sendToSC "s_new" (agentOscMessage agent)
