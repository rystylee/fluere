module Sound.Pulse.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (when)
import Data.Map
import Control.Monad (forM_, void)
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Pulse.PulseData
import Sound.Pulse.PulseMutableMap ( newPulseMMap
                                    ,findValueFromPulseMMap
                                    ,addValToPulseMMap
                                   )
import Sound.Pulse.OSC (sendToSC)
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
newPlayer :: String -> PlayerType -> Int -> [Datum] -> [[Int]] -> PlayerStatus -> Player
newPlayer pname ptype pbpm posc pstream pstatus =
    Player { playerName = pname
            ,playerType = ptype
            ,playerBpm = pbpm
            ,playerOscMessage = posc
            ,playerStream = pstream
            ,playerStatus = pstatus
           }

newPlayerPulseMMap :: Player -> IO (TVar (Map String Player))
newPlayerPulseMMap player = newPulseMMap [(playerName player, player)]

addNewPlayerToPulseMMap :: PulseWorld -> Player -> IO ()
addNewPlayerToPulseMMap world player = do
    let pmmap = wPlayerPulseMMap world
    addValToPulseMMap (playerName player, player) pmmap
--
--

-- These functions are not used during the performance
changePlayer :: PulseWorld -> String -> (Player -> Player) -> IO ()
changePlayer world pname f = do
    let pmmap = wPlayerPulseMMap world
    Just player <- findValueFromPulseMMap pname pmmap
    let newPlayer = f player
    addValToPulseMMap (pname, newPlayer) pmmap

changePlayerStatus :: PulseWorld -> String -> PlayerStatus -> IO ()
changePlayerStatus world pname newpstatus = do
    let changepstatus p = p { playerStatus = newpstatus }
    changePlayer world pname changepstatus

changePlayerStream :: PulseWorld -> String -> [[Int]] -> IO ()
changePlayerStream world pname newstream = do
    let changestream p = p { playerStream = newstream }
    changePlayer world pname changestream

-- Play reguraly according to player's sequence
regularPlay :: PulseWorld -> String -> IO ()
regularPlay world pname = do
    let pmmap = wPlayerPulseMMap world
    Just player <- findValueFromPulseMMap pname pmmap
    let cmmap = wClockPulseMMap world
    Just clock <- findValueFromPulseMMap "defaultClock" cmmap
    let (bpm, stream) = (clockBpm clock, playerStream player)
    forM_ stream $ \stream ->
        forM_ stream $ \node ->
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
play :: PulseWorld -> String -> IO ()
play world pname =
    let pmmap = wPlayerPulseMMap world
        checkPlayerStatus player Playing = (forkIO $ regularPlay world pname) >> return ()
        checkPlayerStatus player Pausing = putStrLn $ playerName player ++ " is pausing."
    in do
        Just player <- findValueFromPulseMMap pname pmmap -- it is need to do Exception handling
        checkPlayerStatus player (playerStatus player)

stopPlayer :: PulseWorld -> String -> IO ()
stopPlayer world pname = do
    let pmmap = wPlayerPulseMMap world
    Just player <- findValueFromPulseMMap pname pmmap
    when (playerStatus player == Playing) $ changePlayerStatus world pname Pausing
--
--
