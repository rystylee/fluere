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
import Sound.Fluere.OSC (sendToSC)
--import Sound.Pulse.Chord


-- Util
--
-- Used to convert bpm to sleep time
-- ex.) bpm:60 -> 25
bpmToSleepTime :: Int -> Int
bpmToSleepTime bpm = 1500 `div` bpm

-- sleep 25 means threadDelay 0.25 * 1000 * 1000
sleep :: Int -> IO ()
sleep t = threadDelay (t * 10 * 1000)
--
--

-- These functions are used to create data with Player
--
-- Used to create a new Player
newPlayer :: String -> PlayerType -> [Datum] -> [[Int]] -> PlayerStatus -> Player
newPlayer pname ptype posc pscore pstatus =
    Player { playerName = pname
            ,playerType = ptype
            ,playerOscMessage = posc
            ,playerScore = pscore
            ,playerStatus = pstatus
           }

newPlayerMMap :: Player -> IO (TVar (Map String Player))
newPlayerMMap player = newMMap [(playerName player, player)]

addNewPlayer :: FluereWorld -> Player -> IO ()
addNewPlayer world player = do
    let pmmap = wPlayerMMap world
    addValToMMap (playerName player, player) pmmap
--
--

-- These functions are not used during the performance
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

-- Play reguraly according to player's sequence
regularPlay :: FluereWorld -> String -> IO ()
regularPlay world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    let cmmap = wClockMMap world
    Just clock <- findValueFromMMap "defaultClock" cmmap
    let (bpm, score) = (clockBpm clock, playerScore player)
    forM_ score $ \music ->
        forM_ music $ \node ->
            if node == 1
                then do
                    sendToSC "s_new" (playerOscMessage player)
                    sleep $ bpmToSleepTime bpm
                else
                    sleep $ bpmToSleepTime bpm
    when (playerStatus player == Playing) $ regularPlay world pname
--
--

-- These functions are used during the performance
play :: FluereWorld -> String -> IO ()
play world pname =
    let pmmap = wPlayerMMap world
        checkPlayerStatus player Playing = (forkIO $ regularPlay world pname) >> return ()
        checkPlayerStatus player Pausing = putStrLn $ playerName player ++ " is pausing."
    in do
        Just player <- findValueFromMMap pname pmmap -- it is need to do Exception handling
        checkPlayerStatus player (playerStatus player)

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
--
--
