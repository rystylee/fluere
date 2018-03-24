module Sound.Fluere.Default where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Fluere.Data
import Sound.Fluere.Clock (currentTime, newClock, newClockMMap)
import Sound.Fluere.Player (newPlayer, newPlayerMMap)
import Sound.Fluere.Action (newAction, newActionMMap, act)
import Sound.Fluere.Pattern (newPattern, newPatternMMap)
import Sound.Fluere.DataBase (newDataBase)


defaultClock :: IO Clock
defaultClock = do
    ct <- currentTime
    let clockName' = "defaultClock"
        tempo' = Tempo { cps = 0.5
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

defaultPlayer :: IO Player
defaultPlayer = do
    let playerName' = "defaultPlayer"
        playerClock' = "defaultClock"
        playerAction' = "defaultAction"
        playerPattern' = "defaultPattern"
        playerOscMessage' = [string "kick1", string "freq", float 440]
        playerStatus' = Playing
        playerBeat' = 0
    return $ newPlayer playerName' playerClock' playerAction' playerPattern' playerOscMessage' playerStatus' playerBeat'

defaultAction :: IO Action
defaultAction = do
    let aname = "defaultAction"
        afunc = act
    return $ newAction aname afunc

defaultPattern :: IO Pattern
defaultPattern = do
    let pname = "defaultPattern"
        interval' = [[4], [4], [1,1,1,1]]
    return $ newPattern pname interval'

defaultDataBase :: IO DataBase
defaultDataBase = do
    clmmap <- defaultClockMMap
    playermmap <- defaultPlayerMMap
    actmmap <- defaultActionMMap
    patternmmap <- defaultPatternMMap
    return $ newDataBase "defaultDB" clmmap playermmap actmmap patternmmap

defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = do
    c <- defaultClock
    newClockMMap c

defaultPlayerMMap :: IO (TVar (Map String Player))
defaultPlayerMMap = do
    p <- defaultPlayer
    newPlayerMMap p

defaultActionMMap :: IO (TVar (Map String Action))
defaultActionMMap = do
    act <- defaultAction
    newActionMMap act

defaultPatternMMap :: IO (TVar (Map String Pattern))
defaultPatternMMap = do
    p <- defaultPattern
    newPatternMMap p
