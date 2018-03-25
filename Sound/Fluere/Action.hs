module Sound.Fluere.Action where

import Control.Concurrent.STM (TVar)
import Data.Map

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.OSC (sendToSC)
import Sound.Fluere.Player (changePlayerStatus)


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
addNewAction db act = addValToMMap (actionName act, act) (actionMMap db)

---------------------------------------------------------------------
-- Different action function for each Player
---------------------------------------------------------------------

act :: String -> DataBase -> String -> IO ()
act playerAction' db pname
    | playerAction' == "playSound" = do
        Just player <- findValueFromMMap pname (playerMMap db)
        sendToSC "s_new" (playerOscMessage player)

    | playerAction' == "swapPlayerStatus" = do
        Just player <- findValueFromMMap pname (playerMMap db)
        if playerStatus player == Playing
            then changePlayerStatus db pname Pausing
            else changePlayerStatus db pname Playing

    | playerAction' == "putStrLn"          = putStrLn "Hello, World!"

    | otherwise = do
        Just player <- findValueFromMMap pname (playerMMap db)
        sendToSC "s_new" (playerOscMessage player)
