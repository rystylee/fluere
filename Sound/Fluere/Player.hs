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
import Sound.Fluere.Clock (currentTime, sleep, getNextEventTime)
import Sound.Fluere.OSC (sendToSC)
--import Sound.Pulse.Chord


-- These functions are used to create data with Player
--
-- Used to create a new Player
newPlayer :: String -> [Datum] -> [[Int]] -> PlayerStatus -> (Int, Int) -> Player
newPlayer pname posc pscore pstatus scorecounter =
    Player { playerName = pname
            ,playerOscMessage = posc
            ,playerScore = pscore
            ,playerStatus = pstatus
            ,scoreCounter = scorecounter
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

changeScoreCounter :: FluereWorld -> String -> (Int, Int) -> IO ()
changeScoreCounter world pname newscorecounter = do
    let changescorecounter p = p { scoreCounter = newscorecounter }
    changePlayer world pname changescorecounter

-- Used to get next note
getNextNote :: FluereWorld -> String -> IO Int
getNextNote world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    let pscore = playerScore player
        scorecounter = scoreCounter player
    return $ searchNote pscore scorecounter

searchNote :: [[Int]] -> (Int, Int) -> Int
searchNote pscore scorecounter =
    let (row, column) = scorecounter
    in (pscore !! row) !! column

updateScoreCounter :: FluereWorld -> String -> IO ()
updateScoreCounter world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    let pscore = playerScore player
        scorecounter = scoreCounter player
        (row, column) = scorecounter
        rowLength = length pscore
    if column < 3
        then do
            let newcolumn = column + 1
                newrow = row
            changeScoreCounter world pname (newrow, newcolumn)
            --return $ (newrow, newcolumn)
        else if row < (rowLength - 1)
            then do
                let newcolumn = 0
                    newrow = row + 1
                changeScoreCounter world pname (newrow, newcolumn)
                --return $ (newrow, newcolumn)
            else do
                let newcolumn = 0
                    newrow = 0
                changeScoreCounter world pname (newrow, newcolumn)
                --return $ (newrow, newcolumn)


-- Used to play a Player
play :: FluereWorld -> String -> IO ()
play world pname =
    let pmmap = wPlayerMMap world
        checkPlayerStatus player Playing = (forkIO $ basicPlay world pname) >> return ()
        checkPlayerStatus player Pausing = putStrLn $ playerName player ++ " is pausing."
    in do
        Just player <- findValueFromMMap pname pmmap -- it is need to do Exception handling
        checkPlayerStatus player (playerStatus player)

basicPlay :: FluereWorld -> String -> IO ()
basicPlay world pname = do
    let pmmap = wPlayerMMap world
    Just player <- findValueFromMMap pname pmmap
    nt <- getNextEventTime world "defaultClock"
    ct <- currentTime
    if nt > ct
        then do
            let diff = (nt - ct)
            sleep diff
        else do
            note <- getNextNote world pname
            if note == 1
                then do
                    forkIO $ sendToSC "s_new" (playerOscMessage player) >> return ()
                else do
                    forkIO $ sleep 0.01 >> return ()
            updateScoreCounter world pname
            nt' <- getNextEventTime world "defaultClock"
            ct' <- currentTime
            sleep (ct' - nt')
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
