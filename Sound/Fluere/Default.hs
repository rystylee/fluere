module Sound.Fluere.Default where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Fluere.Data
import Sound.Fluere.Clock (currentTime, newClock, newClockMMap)
import Sound.Fluere.Agent (newAgent, newAgentMMap)
import Sound.Fluere.Action (newAction, newActionMMap, playSound)
import Sound.Fluere.Pattern (newPattern, newPatternMMap)
import Sound.Fluere.DataBase (newDataBase)


defaultClock :: IO Clock
defaultClock = do
    ct <- currentTime
    let clockName' = "defaultClock"
        tempo' = Tempo {
             cps = 0.5
            ,beat = 4
        }
        tempohistory = TempoHistory {
             tempo = tempo'
            ,startTime = ct
            ,startBar = 0
            ,startBeat = 0
            ,lastBar = 0
            ,lastBeat = 0
        }
    return $ newClock clockName' [tempohistory]

defaultAgent :: IO Agent
defaultAgent = do
    let agentName' = "defaultAgent"
        agentClock' = "defaultClock"
        agentAction' = "defaultAction"
        agentPattern' = "defaultPattern"
        agentOscMessage' = [string "kick1", string "freq", float 440]
        agentStatus' = Playing
        agentBeat' = 0
    return $ newAgent agentName' agentClock' agentAction' agentPattern' agentOscMessage' agentStatus' agentBeat'

defaultAction :: IO Action
defaultAction = do
    let aname = "defaultAction"
        f = playSound
    return $ newAction aname f

defaultPattern :: IO Pattern
defaultPattern = do
    let pname = "defaultPattern"
        interval' = [4,4,1,1,1,1]
    return $ newPattern pname interval'

defaultDataBase :: IO DataBase
defaultDataBase = do
    cmmap <- defaultClockMMap
    ammap <- defaultAgentMMap
    actmmap <- defaultActionMMap
    pmmap <- defaultPatternMMap
    return $ newDataBase "defaultDB" cmmap ammap actmmap pmmap

defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = do
    c <- defaultClock
    newClockMMap c

defaultAgentMMap :: IO (TVar (Map String Agent))
defaultAgentMMap = do
    a <- defaultAgent
    newAgentMMap a

defaultActionMMap :: IO (TVar (Map String Action))
defaultActionMMap = do
    act <- defaultAction
    newActionMMap act

defaultPatternMMap :: IO (TVar (Map String Pattern))
defaultPatternMMap = do
    p <- defaultPattern
    newPatternMMap p
