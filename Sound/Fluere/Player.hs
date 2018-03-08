module Sound.Fluere.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (when)
import Data.Map
import Control.Monad (forM_, void)
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Clock ( sleep
                           ,beatToTime
                           ,barToBeat
                           ,currentTime
                           ,currentBar
                           ,currentBeat
                           ,currentTempoHistory
                           ,elapsedTimeOfBar
                           ,elapsedTimeOfBeat
                           ,checkTempoChange 
                          ) 
import Sound.Fluere.OSC (sendToSC)


------------------------------------------------------
-- For debug
------------------------------------------------------

displayPlayer :: DataBase -> String -> IO ()
displayPlayer db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    putStrLn $ "\n------------------------------------" 
    putStrLn $ "playerName : " ++ show (playerName player)
    putStrLn $ "beatToStart : " ++ show (beatToStart player)
    putStrLn $ "------------------------------------\n" 

------------------------------------------------------

-- Used to create a new Player
newPlayer :: String -> [Datum] -> [[Int]] -> PlayerStatus -> Double -> (Int, Int) -> Player
newPlayer pname posc pscore pstatus beatToStart' scorecounter =
    Player { playerName = pname
            ,playerOscMessage = posc
            ,playerScore = pscore
            ,playerStatus = pstatus
            ,beatToStart = beatToStart'
            ,scoreCounter = scorecounter
           }

-- Used to create a new Player MutableMap
newPlayerMMap :: Player -> IO (TVar (Map String Player))
newPlayerMMap player = newMMap [(playerName player, player)]

-- Used to add a new Player to MutableMap
addNewPlayer :: DataBase -> Player -> IO ()
addNewPlayer db player = do
    let pmmap = playerMMap db
    addValToMMap (playerName player, player) pmmap


-- The base function to change Player
changePlayer :: DataBase -> String -> (Player -> Player) -> IO ()
changePlayer db pname f = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let newPlayer = f player
    addValToMMap (pname, newPlayer) pmmap

changePlayerStatus :: DataBase -> String -> PlayerStatus -> IO ()
changePlayerStatus db pname newpstatus = do
    let changepstatus p = p { playerStatus = newpstatus }
    changePlayer db pname changepstatus

changePlayerScore :: DataBase -> String -> [[Int]] -> IO ()
changePlayerScore db pname newscore = do
    let changescore p = p { playerScore = newscore }
    changePlayer db pname changescore

changeScoreCounter :: DataBase -> String -> (Int, Int) -> IO ()
changeScoreCounter db pname newscorecounter = do
    let changescorecounter p = p { scoreCounter = newscorecounter }
    changePlayer db pname changescorecounter

changeBeatToStart :: DataBase -> String -> Double -> IO ()
changeBeatToStart db pname newbeattostart = do
    let changebeattostart p = p { beatToStart = newbeattostart }
    changePlayer db pname changebeattostart


-- Used to get next note
getNextNote :: DataBase -> String -> IO Int
getNextNote db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let pscore = playerScore player
        scorecounter = scoreCounter player
    return $ searchNote pscore scorecounter

searchNote :: [[Int]] -> (Int, Int) -> Int
searchNote pscore scorecounter =
    let (row, column) = scorecounter
    in (pscore !! row) !! column

updateScoreCounter :: DataBase -> String -> IO ()
updateScoreCounter db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let pscore = playerScore player
        (row, col) = scoreCounter player
        rowLength = length pscore
    if col < 3
        then do
            let newcol = col + 1
                newrow = row
            changeScoreCounter db pname (newrow, newcol)
        else if row < (rowLength - 1)
            then do
                let newcol = 0
                    newrow = row + 1
                changeScoreCounter db pname (newrow, newcol)
            else do
                let newcol = 0
                    newrow = 0
                changeScoreCounter db pname (newrow, newcol)

play :: DataBase -> String -> IO ()
play db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    if (playerStatus player == Playing)
        then void (forkIO $ basicPlay db pname)
        else putStrLn $ playerName player ++ " is pausing."

playAt :: DataBase -> String -> Double -> IO ()
playAt db pname beat' = do
    changeBeatToStart db pname beat'
    play db pname

basicPlay :: DataBase -> String -> IO ()
basicPlay db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let cmmap = clockMMap db
    Just clock <- findValueFromMMap "defaultClock" cmmap
    cb <- currentBeat clock
    let bs = beatToStart player
    if (bs > cb)
        then do
            ct <- currentTime
            nt <- elapsedTimeOfBeat clock bs
            let diff = nt - ct
            sleep $ diff
        else do
            note <- getNextNote db pname
            when (note == 1) $ void (forkIO $ sendToSC "s_new" (playerOscMessage player))
            updateScoreCounter db pname
            beatOfNextEvent <- checkTempoChange db "defaultClock"
            changeBeatToStart db pname beatOfNextEvent
            --putStrLn $ "beatOfNextEvent : " ++ show beatOfNextEvent
            --ct <- currentTime
            --nt <- elapsedTimeOfBeat clock beatOfNextEvent
            --let diff = nt - ct
            --sleep $ diff
            sleep $ beatToTime clock (beatOfNextEvent - cb)
    when (playerStatus player == Playing) $ basicPlay db pname

startPlayer :: DataBase -> String -> IO ()
startPlayer db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    when (playerStatus player == Pausing) $ changePlayerStatus db pname Playing

stopPlayer :: DataBase -> String -> IO ()
stopPlayer db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    when (playerStatus player == Playing) $ changePlayerStatus db pname Pausing

playPlayers :: DataBase -> [String] -> IO ()
playPlayers db pnames = mapM_ (play db) pnames

startPlayers :: DataBase -> [String] -> IO ()
startPlayers db pnames = mapM_ (startPlayer db) pnames

stopPlayers :: DataBase -> [String] -> IO ()
stopPlayers db pnames = mapM_ (stopPlayer db) pnames
