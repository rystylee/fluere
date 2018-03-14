module Sound.Fluere.Agent where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_, void, when)
import Data.Map
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Clock ( sleep
                           ,beatToTime
                           ,currentBeat
                          )
import Sound.Fluere.Pattern ( nextBeat
                            )


------------------------------------------------------
-- For debug
------------------------------------------------------

displayAgent :: DataBase -> String -> IO ()
displayAgent db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    putStrLn $ "\n------------------------------------" 
    putStrLn $ "agentName : " ++ show (agentName agent)
    putStrLn $ "agentClock : " ++ show (agentClock agent)
    putStrLn $ "agentAction : " ++ show (agentAction agent)
    putStrLn $ "agentPattern : " ++ show (agentPattern agent)
    putStrLn $ "agentBeat : " ++ show (agentBeat agent)
    putStrLn $ "------------------------------------\n" 

------------------------------------------------------

-- Used to create a new Agent
newAgent :: String -> String -> String -> String -> [Datum] -> AgentStatus -> Double -> Agent
newAgent aname aclock aaction apattern aosc astatus abeat = Agent {
     agentName = aname
    ,agentClock = aclock
    ,agentAction = aaction
    ,agentPattern = apattern
    ,agentOscMessage = aosc
    ,agentStatus = astatus
    ,agentBeat = abeat
}

-- Used to create a new Agent MutableMap
newAgentMMap :: Agent -> IO (TVar (Map String Agent))
newAgentMMap agent = newMMap [(agentName agent, agent)]

-- Used to add a new Agent to MutableMap
addNewAgent :: DataBase -> Agent -> IO ()
addNewAgent db agent = do
    addValToMMap (agentName agent, agent) (agentMMap db)


-- The base function to change Agent
changeAgent :: DataBase -> String -> (Agent -> Agent) -> IO ()
changeAgent db aname f = do
    let ammap = agentMMap db
    Just agent <- findValueFromMMap aname ammap
    let newAgent = f agent
    addValToMMap (aname, newAgent) ammap

changeAgentClock :: DataBase -> String -> String -> IO ()
changeAgentClock db aname newclock= do
    let changeclock a = a { agentClock = newclock }
    changeAgent db aname changeclock

changeAgentAction :: DataBase -> String -> String -> IO ()
changeAgentAction db aname newaction = do
    let changeaction a = a { agentAction = newaction }
    changeAgent db aname changeaction

changeAgentPattern :: DataBase -> String -> String -> IO ()
changeAgentPattern db aname newpattern = do
    let changepattern a = a { agentPattern = newpattern }
    changeAgent db aname changepattern

changeAgentStatus :: DataBase -> String -> AgentStatus -> IO ()
changeAgentStatus db aname newastatus = do
    let changeastatus a = a { agentStatus = newastatus }
    changeAgent db aname changeastatus

changeAgentBeat :: DataBase -> String -> Double -> IO ()
changeAgentBeat db aname newbeat = do
    let changebeat a = a { agentBeat = newbeat }
    changeAgent db aname changebeat

------------------------------------------------------

act :: DataBase -> String -> IO ()
act db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    if (agentStatus agent == Playing)
        then void (forkIO $ actLoop db aname)
        else putStrLn $ agentName agent ++ " is pausing."

actAt :: DataBase -> String -> Double -> IO ()
actAt db aname beat' = do
    changeAgentBeat db aname beat'
    act db aname

actLoop :: DataBase -> String -> IO ()
actLoop db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    Just clock <- findValueFromMMap (agentClock agent) (clockMMap db)
    cb <- currentBeat clock
    let bs = agentBeat agent
    if (bs > cb)
        then do
            let diff = bs - cb
            sleep $ diff
        else do
            Just act <- findValueFromMMap (agentAction agent) (actionMMap db)
            void $ forkIO $ (actionFunc act) db aname
            beatOfNextEvent <- nextBeat db aname cb
            changeAgentBeat db aname (beatOfNextEvent + cb)
            sleep $ beatToTime clock beatOfNextEvent
    when (agentStatus agent == Playing) $ actLoop db aname

startAgent :: DataBase -> String -> IO ()
startAgent db aname = do
    let ammap = agentMMap db
    Just agent <- findValueFromMMap aname ammap
    when (agentStatus agent == Pausing) $ changeAgentStatus db aname Playing

stopAgent :: DataBase -> String -> IO ()
stopAgent db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    when (agentStatus agent == Playing) $ changeAgentStatus db aname Pausing

actAgents :: DataBase -> [String] -> IO ()
actAgents db anames = mapM_ (act db) anames

startAgents :: DataBase -> [String] -> IO ()
startAgents db anames = mapM_ (startAgent db) anames

stopAgents :: DataBase -> [String] -> IO ()
stopAgents db anames = mapM_ (stopAgent db) anames
