module Sound.Fluere.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (when)
import Data.Map
import Control.Monad (forM_, void)
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Fluere.BaseData
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Time (currentTime, sleep, beatToDeltaByBpm)
import Sound.Fluere.OSC (sendToSC)
--import Sound.Pulse.Chord


-- These functions are used to create data with Player
--
-- Used to create a new Player
newPlayer :: String -> [Datum] -> [[Int]] -> PlayerStatus -> Player
newPlayer pname posc pscore pstatus =
    Player { playerName = pname
            ,playerOscMessage = posc
            ,playerScore = pscore
            ,playerStatus = pstatus
           }

-- Used to create a new Player MutableMap
newPlayerMMap :: Player -> IO (TVar (Map String Player))
newPlayerMMap player = newMMap [(playerName player, player)]

-- Used to add a new Player to MutableMap
addNewPlayer :: FluereWorld -> Player -> IO ()
addNewPlayer world player = do
    let pmmap = wPlayerMMap world
    addValToMMap (playerName player, player) pmmap
--
--

-- The base function to change Player
changePlayer :: FluereWorld -> String -> (Player -> Player) -> IO ()
changePlayer world pname f = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    let newPlayer = f player
    addValToMMap (pname, newPlayer) pmmap

changePlayerStatus :: FluereWorld -> String -> PlayerStatus -> IO ()
changePlayerStatus world pname newpstatus = do
    let changepstatus p = p { playerStatus = newpstatus }
    changePlayer world pname changepstatus

changePlayerScore :: FluereWorld -> String -> [[Int]] -> IO ()
changePlayerScore world pname newscore = do
    let changescore p = p { playerScore = newscore }
    changePlayer world pname changescore


-- Used to play a Player
play :: FluereWorld -> String -> IO ()
play world pname =
    let pmmap = wPlayerMMap world
        checkPlayerStatus player Playing = (forkIO $ basicPlay world pname) >> return ()
        checkPlayerStatus player Pausing = putStrLn $ playerName player ++ " is pausing."
    in do
        Just player <- findValueFromMMap pname pmmap -- it is need to do Exception handling
        checkPlayerStatus player (playerStatus player)

-- Play reguraly according to player's sequence
--basicPlay :: FluereWorld -> String -> IO ()
--basicPlay world pname = do
--    let pmmap = wPlayerMMap world
--    Just player <- findValueFromMMap pname pmmap
--    let cmmap = wClockMMap world
--    Just clock <- findValueFromMMap "defaultClock" cmmap
--    let (bpm, score) = (clockBpm clock, playerScore player)
--    forM_ score $ \music ->
--        forM_ music $ \node ->
--            if node == 1
--                then do
--                    sendToSC "s_new" (playerOscMessage player)
--                    sleep $ bpmToSleepTime bpm
--                else
--                    sleep $ bpmToSleepTime bpm
--    when (playerStatus player == Playing) $ basicPlay world pname

basicPlay :: FluereWorld -> String -> IO ()
basicPlay world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap "defaultClock" cmmap
    let nt = nextEventTime clock
    ct <- currentTime
    if (nt > ct)
        then do
            let diff = (nt - ct)
            sleep diff
        else do
            sendToSC "s_new" (playerOscMessage player)
            let bpm = clockBpm clock
                beat = clockBeat clock
                delta = beatToDeltaByBpm bpm beat
            sleep delta
    when (playerStatus player == Playing) $ basicPlay world pname


startPlayer :: FluereWorld -> String -> IO ()
startPlayer world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    when (playerStatus player == Pausing) $ changePlayerStatus world pname Playing

stopPlayer :: FluereWorld -> String -> IO ()
stopPlayer world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    when (playerStatus player == Playing) $ changePlayerStatus world pname Pausing

playPlayers :: FluereWorld -> [String] -> IO ()
playPlayers world pnames = mapM_ (play world) pnames

startPlayers :: FluereWorld -> [String] -> IO ()
startPlayers world pnames = mapM_ (startPlayer world) pnames

stopPlayers :: FluereWorld -> [String] -> IO ()
stopPlayers world pnames = mapM_ (stopPlayer world) pnames
