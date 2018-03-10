module Sound.Fluere.Default where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Fluere.Data
import Sound.Fluere.Clock (currentTime, newClock, newClockMMap)
import Sound.Fluere.Agent (newAgent, newAgentMMap)
import Sound.Fluere.DataBase (newDataBase)


defaultClock :: IO Clock
defaultClock = do
    ct <- currentTime
    let clockName' = "defaultClock"
        tempo' = Tempo { cps = 1
                        ,beat = 4
                       }
        tempohistory = TempoHistory { tempo = tempo'
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
        agentClock = "defaultClock"
        agentOscMessage' = [string "kick1", string "freq", float 440]
        agentScore' = [[1,0,1,0], [1,1,1,1]]
        agentStatus' = Playing
        beatToStart' = 0
        scoreCounter' = (0, 0) :: (Int, Int)
    return $ newAgent agentClock agentName' agentOscMessage' agentScore' agentStatus' beatToStart' scoreCounter'

defaultDataBase :: IO DataBase
defaultDataBase = do
    cmmap <- defaultClockMMap
    ammap <- defaultAgentMMap
    return $ newDataBase "defaultDB" cmmap ammap

defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = do
    c <- defaultClock
    newClockMMap c

defaultAgentMMap :: IO (TVar (Map String Agent))
defaultAgentMMap = do
    a <- defaultAgent
    newAgentMMap a
