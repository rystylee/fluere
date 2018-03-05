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
import Sound.Fluere.Clock (currentTime, currentBeat, sleep, beatToDelta, beatToTime)
import Sound.Fluere.OSC (sendToSC)


-- Used to create a new Player
newPlayer :: String -> [Datum] -> [[Int]] -> PlayerStatus -> Double -> (Int, Int) -> Player
newPlayer pname posc pscore pstatus lastBeat' scorecounter =
    Player { playerName = pname
            ,playerOscMessage = posc
            ,playerScore = pscore
            ,playerStatus = pstatus
            ,lastBeat = lastBeat'
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

changeLastBeat :: DataBase -> String -> Double -> IO ()
changeLastBeat db pname newlastbeat = do
    let changelastbeat p = p { lastBeat = newlastbeat }
    changePlayer db pname changelastbeat


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
        scorecounter = scoreCounter player
        (row, column) = scorecounter
        rowLength = length pscore
    if column < 3
        then do
            let newcolumn = column + 1
                newrow = row
            changeScoreCounter db pname (newrow, newcolumn)
            --return $ (newrow, newcolumn)
        else if row < (rowLength - 1)
            then do
                let newcolumn = 0
                    newrow = row + 1
                changeScoreCounter db pname (newrow, newcolumn)
                --return $ (newrow, newcolumn)
            else do
                let newcolumn = 0
                    newrow = 0
                changeScoreCounter db pname (newrow, newcolumn)
                --return $ (newrow, newcolumn)


play :: DataBase -> String -> IO ()
play db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap -- it is need to do Exception handling
    let cmmap = clockMMap db
    Just clock <- findValueFromMMap "defaultClock" cmmap
    cb <- currentBeat clock
    if (playerStatus player == Playing)
        then changeLastBeat db pname cb >> (forkIO $ basicPlay db pname) >> return ()
        else putStrLn $ playerName player ++ " is pausing."


basicPlay :: DataBase -> String -> IO ()
basicPlay db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let cmmap = clockMMap db
    Just clock <- findValueFromMMap "defaultClock" cmmap
    cb <- currentBeat clock
    let lb = lastBeat player
    if (cb /= (lb + 1))
        then do
            ct <- currentTime
            nt <- beatToTime clock (lb + 1)
            let diff = nt - ct
            sleep $ diff
        else do
            note <- getNextNote db pname
            if note == 1
                then do
                    forkIO $ sendToSC "s_new" (playerOscMessage player) >> return ()
                else do
                    forkIO $ return ()
            updateScoreCounter db pname
            changeLastBeat db pname cb
            ct <- currentTime
            nt <- beatToTime clock (cb + 1)
            let diff = nt - ct
            sleep $ diff
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
