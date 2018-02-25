module Sound.Pulse.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Data.Map
import Control.Monad (forM_, void)
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Pulse.PulseData
import Sound.Pulse.PulseMutableMap (newPulseMMap, findValueFromPulseMMap)
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

--newPlayerPulseMMap :: Ord => k -> IO Player -> IO (TVar (Map k IO Player))
newPlayerPulseMMap :: String -> Player -> IO (TVar (Map String Player))
newPlayerPulseMMap key player = newPulseMMap [(key, player)]
--
--

-- These functions are used to change Player
--
--changePlayer
--changePlayer ::
--changePlayer player =

-- Used to play a player
playPlayer :: Player -> IO ()
playPlayer player = 
    if playerStatus player == Pausing
        then putStrLn "Player Starts."
        else putStrLn "Player has been playing." 

-- Used to pause a player
pausePlayer :: Player -> IO ()
pausePlayer player =
    if playerStatus player == Playing
        then putStrLn "Player Pauses."
        else putStrLn "Player has been pausing." 
--
--

-- Some functions to play with Player
--
--play :: Player -> IO ()
play :: PulseWorld -> String -> IO ()
play world pname =
    let pmmap = wPlayerPulseMMap world
        checkPlayerStatus player Playing = (forkIO $ regularPlay player) >> return ()
        checkPlayerStatus player Pausing = putStrLn $ playerName player ++ " is pausing."
    in do
        Just player <- findValueFromPulseMMap pname pmmap -- it is need to do Exception handling
        checkPlayerStatus player (playerStatus player)


-- Play reguraly according to player's sequence
regularPlay :: Player -> IO ()
regularPlay player = do
    let (bpm, stream) = (playerBpm player, playerStream player)
    forM_ stream $ \stream ->
        forM_ stream $ \node ->
            if node == 1
                then do
                    sendToSC "s_new" (playerOscMessage player) 
                    sleep $ bpmToSleepTime (playerBpm player)
                else 
                    sleep $ bpmToSleepTime (playerBpm player)
    regularPlay player
--
--

{-
playChord' :: Int -> IO ()
playChord' delayTime = do
    forM_ chords $ \chord -> do
        forM_ chord $ \scale -> do
            play "imp" scale
            putStrLn "playChord' done"
        threadDelay (delayTime * 1000 * 1000)
    playChord' delayTime

performe = do
    p <- forkIO $ playChord' 1
    u <- delayy
    when u $ killThread p
    putStrLn "killed the playChord' thread"
-}
