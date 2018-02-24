module Sound.Pulse.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Map
import Control.Monad (forM_, void)
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Pulse.PulseMutableMap
import Sound.Pulse.OSC
import Sound.Pulse.Chord


data Player = Player { playerName :: String
                      ,playerType :: PlayerType
                      ,playerBpm :: Int
                      ,playerOscMessage :: [Datum]
                      ,playerStream :: [[Int]]
                      ,playerStatus :: PlayerStatus
                     } deriving (Show)

data PlayerType =   Regular
                  | Debug deriving (Show, Eq)

data PlayerStatus =   Playing
                    | Pausing deriving (Show, Eq)


-- Used to convert bpm to sleep time
-- ex.) bpm:60 -> 25
bpmToSleepTime :: Int -> Int
bpmToSleepTime bpm = 1500 `div` bpm

-- sleep 25 means threadDelay 0.25 * 1000 * 1000
sleep :: Int -> IO ()
sleep t = threadDelay (t * 10 * 1000)


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

--newPlayerMMap :: Ord k => k -> String -> playerType -> Int -> [Datum] -> [[Int]] -> playerStatus -> IO (TVar (Map k (IO Player)))
--newPlayerMMap key pname ptype pbpm posc pstream pstatus = 
--    let player = newPlayer pname ptype pbpm posc pstream pstatus
--    in newPulseMMap [(key, player)]

--newPlayerPulseMMap :: Ord => k -> IO Player -> IO (TVar (Map k IO Player))
newPlayerPulseMMap key player = newPulseMMap [(key, player)]


--changePlayer
--changePlayer ::
--changePlayer player =

-- Used to play a player
playPlayer :: Player -> IO ()
playPlayer player = do
    if playerStatus player == Pausing
        then putStrLn "Player Starts."
        else putStrLn "Player has been playing." 

-- Used to pause a player
pausePlayer :: Player -> IO ()
pausePlayer player = do
    if playerStatus player == Playing
        then putStrLn "Player Pauses."
        else putStrLn "Player has been pausing." 


-- Used to play a player
-- play function is base function
--play :: IO Player -> IO ()
--play :: IO Player -> IO GHC.Conc.Sync.ThreadId
play player =
    if playerStatus player == Playing
        then do
            let ptype = playerType player
            case ptype of
                Regular -> forkIO $ regularPlay player
                _ -> forkIO $ putStr "Playre type error."
            play player
        else do
            forkIO $ putStrLn $ playerName player ++ " is pausing."
            play player

-- Play reguraly according to player's sequence
--regularPlay :: IO Player -> IO ()
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
    --regularPlay player

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
