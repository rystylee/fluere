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


------------------------------------------------------
-- For debug
------------------------------------------------------

displayAgent :: DataBase -> String -> IO ()
displayAgent db aname = do
    let ammap = agentMMap db
    Just agent <- findValueFromMMap aname ammap
    putStrLn $ "\n------------------------------------" 
    putStrLn $ "agentName : " ++ show (agentName agent)
    putStrLn $ "beatToStart : " ++ show (beatToStart agent)
    putStrLn $ "------------------------------------\n" 

------------------------------------------------------

-- Used to create a new Agent
newAgent :: String -> String -> String -> [Datum] -> [[Int]] -> AgentStatus -> Double -> (Int, Int) -> Agent
newAgent aname aclock aaction aosc ascore astatus beatToStart' scorecounter =
    Agent { agentName = aname
           ,agentClock = aclock
           ,agentAction = aaction
           ,agentOscMessage = aosc
           ,agentScore = ascore
           ,agentStatus = astatus
           ,beatToStart = beatToStart'
           ,scoreCounter = scorecounter
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

changeAgentStatus :: DataBase -> String -> AgentStatus -> IO ()
changeAgentStatus db aname newastatus = do
    let changeastatus a = a { agentStatus = newastatus }
    changeAgent db aname changeastatus

changeAgentScore :: DataBase -> String -> [[Int]] -> IO ()
changeAgentScore db aname newscore = do
    let changescore a = a { agentScore = newscore }
    changeAgent db aname changescore

changeScoreCounter :: DataBase -> String -> (Int, Int) -> IO ()
changeScoreCounter db aname newscorecounter = do
    let changescorecounter a = a { scoreCounter = newscorecounter }
    changeAgent db aname changescorecounter

changeBeatToStart :: DataBase -> String -> Double -> IO ()
changeBeatToStart db aname newbeattostart = do
    let changebeattostart a = a { beatToStart = newbeattostart }
    changeAgent db aname changebeattostart


-- Used to get next note
getNextNote :: DataBase -> String -> IO Int
getNextNote db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    let ascore = agentScore agent
        scorecounter = scoreCounter agent
    return $ searchNote ascore scorecounter

searchNote :: [[Int]] -> (Int, Int) -> Int
searchNote ascore scorecounter =
    let (row, column) = scorecounter
    in (ascore !! row) !! column

updateScoreCounter :: DataBase -> String -> IO ()
updateScoreCounter db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    let ascore = agentScore agent
        (row, col) = scoreCounter agent
        rowLength = length ascore
    if col < 3
        then do
            let newcol = col + 1
                newrow = row
            changeScoreCounter db aname (newrow, newcol)
        else if row < (rowLength - 1)
            then do
                let newcol = 0
                    newrow = row + 1
                changeScoreCounter db aname (newrow, newcol)
            else do
                let newcol = 0
                    newrow = 0
                changeScoreCounter db aname (newrow, newcol)

act :: DataBase -> String -> IO ()
act db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    if (agentStatus agent == Playing)
        then void (forkIO $ actLoop db aname)
        else putStrLn $ agentName agent ++ " is pausing."

actAt :: DataBase -> String -> Double -> IO ()
actAt db aname beat' = do
    changeBeatToStart db aname beat'
    act db aname

actLoop :: DataBase -> String -> IO ()
actLoop db aname = do
    Just agent <- findValueFromMMap aname (agentMMap db)
    Just clock <- findValueFromMMap (agentClock agent) (clockMMap db)
    cb <- currentBeat clock
    let bs = beatToStart agent
    if (bs > cb)
        then do
            let diff = bs - cb
            sleep $ diff
        else do
            note <- getNextNote db aname
            Just act <- findValueFromMMap (agentAction agent) (actionMMap db)
            when (note == 1) $ void (forkIO $ (subStance act) db aname)
            updateScoreCounter db aname
            let beatOfNextEvent = cb + 1
            changeBeatToStart db aname beatOfNextEvent
            sleep $ beatToTime clock (beatOfNextEvent - cb)
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