module Sound.Fluere.Default where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum, string, float)

import Sound.Fluere.Data
import Sound.Fluere.Clock (currentTime, newClock, newClockMMap)
import Sound.Fluere.Player (newPlayer, newPlayerMMap)
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
                                    }
    return $ newClock clockName' [tempohistory]

defaultPlayer :: IO Player
defaultPlayer = do
    let playerName' = "defaultPlayer"
        playerOscMessage' = [string "kick1", string "freq", float 440]
        playerScore' = [[1,0,1,0], [1,1,1,1]]
        playerStatus' = Playing
        lastBeat' = 0
        scoreCounter' = (0, 0) :: (Int, Int)
    return $ newPlayer playerName' playerOscMessage' playerScore' playerStatus' lastBeat' scoreCounter'

defaultDataBase :: IO DataBase
defaultDataBase = do
    cmmap <- defaultClockMMap
    pmmap <- defaultPlayerMMap
    return $ newDataBase "defaultDB" cmmap pmmap

defaultClockMMap :: IO (TVar (Map String Clock))
defaultClockMMap = do
    c <- defaultClock
    newClockMMap c

defaultPlayerMMap :: IO (TVar (Map String Player))
defaultPlayerMMap = do
    p <- defaultPlayer
    newPlayerMMap p
