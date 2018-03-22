module Sound.Fluere.Player where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_, void, when)
import Data.Map
import Sound.OSC.FD (Datum, string, int32, float)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )
import Sound.Fluere.Clock ( sleep
                           ,beatToTime
                           ,currentBeat
                          )
import Sound.Fluere.Pattern (nextBeat)


---------------------------------------------------------------------
-- For debug
---------------------------------------------------------------------

displayPlayer :: DataBase -> String -> IO ()
displayPlayer db pname = do
    Just player <- findValueFromMMap pname (playerMMap db)
    putStrLn $ "\n------------------------------------" 
    putStrLn $ "playerName : " ++ show (playerName player)
    putStrLn $ "playerClock : " ++ show (playerClock player)
    putStrLn $ "playerAction : " ++ show (playerAction player)
    putStrLn $ "playerPattern : " ++ show (playerPattern player)
    putStrLn $ "playerBeat : " ++ show (playerBeat player)
    putStrLn $ "------------------------------------\n"

---------------------------------------------------------------------

-- Used to create a new Player
newPlayer :: String -> String -> String -> String -> [Datum] -> PlayerStatus -> Double -> Player
newPlayer pname pclock paction ppattern posc pstatus pbeat =
    Player { playerName = pname
            ,playerClock = pclock
            ,playerAction = paction
            ,playerPattern = ppattern
            ,playerOscMessage = posc
            ,playerStatus = pstatus
            ,playerBeat = pbeat
           }

-- Used to create a new Player MutableMap
newPlayerMMap :: Player -> IO (TVar (Map String Player))
newPlayerMMap player = newMMap [(playerName player, player)]

-- Used to add a new Player to MutableMap
addNewPlayer :: DataBase -> Player -> IO ()
addNewPlayer db player = do
    addValToMMap (playerName player, player) (playerMMap db)


-- The base function to change Player
changePlayer :: DataBase -> String -> (Player -> Player) -> IO ()
changePlayer db pname f = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    let newPlayer = f player
    addValToMMap (pname, newPlayer) pmmap

changePlayerClock :: DataBase -> String -> String -> IO ()
changePlayerClock db pname newclock= do
    let changeclock p = p { playerClock = newclock }
    changePlayer db pname changeclock

changePlayerAction :: DataBase -> String -> String -> IO ()
changePlayerAction db pname newaction = do
    let changeaction p = p { playerAction = newaction }
    changePlayer db pname changeaction

changePlayerPattern :: DataBase -> String -> String -> IO ()
changePlayerPattern db pname newpattern = do
    let changepattern p = p { playerPattern = newpattern }
    changePlayer db pname changepattern

changePlayerStatus :: DataBase -> String -> PlayerStatus -> IO ()
changePlayerStatus db pname newastatus = do
    let changeastatus p = p { playerStatus = newastatus }
    changePlayer db pname changeastatus

changePlayerBeat :: DataBase -> String -> Double -> IO ()
changePlayerBeat db pname newbeat = do
    let changebeat p = p { playerBeat = newbeat }
    changePlayer db pname changebeat

---------------------------------------------------------------------

play :: DataBase -> String -> IO ()
play db pname = do
    Just player <- findValueFromMMap pname (playerMMap db)
    if (playerStatus player == Playing)
        then void (forkIO $ playLoop db pname)
        else putStrLn $ playerName player ++ " is pausing."

playAt :: DataBase -> String -> Double -> IO ()
playAt db pname beat' = do
    changePlayerBeat db pname beat'
    play db pname

playLoop :: DataBase -> String -> IO ()
playLoop db pname = do
    Just player <- findValueFromMMap pname (playerMMap db)
    Just clock <- findValueFromMMap (playerClock player) (clockMMap db)
    cb <- currentBeat clock
    let bs = playerBeat player
    if (bs > cb)
        then do
            let diff = bs - cb
            sleep $ diff
        else do
            Just action' <- findValueFromMMap (playerAction player) (actionMMap db)
            void $ forkIO $ (actionFunc action') (playerAction player) db pname
            beatOfNextEvent <- nextBeat db pname cb
            changePlayerBeat db pname (beatOfNextEvent + cb)
            -- Delay countermeasures considered to be caused by GC etc.
            sleep $ (beatToTime clock beatOfNextEvent - 0.01)
    when (playerStatus player == Playing) $ playLoop db pname

startPlayer :: DataBase -> String -> IO ()
startPlayer db pname = do
    let pmmap = playerMMap db
    Just player <- findValueFromMMap pname pmmap
    when (playerStatus player == Pausing) $ changePlayerStatus db pname Playing

stopPlayer :: DataBase -> String -> IO ()
stopPlayer db pname = do
    Just player <- findValueFromMMap pname (playerMMap db)
    when (playerStatus player == Playing) $ changePlayerStatus db pname Pausing

playPlayers :: DataBase -> [String] -> IO ()
playPlayers db pnames = mapM_ (play db) pnames

startPlayers :: DataBase -> [String] -> IO ()
startPlayers db pnames = mapM_ (startPlayer db) pnames

stopPlayers :: DataBase -> [String] -> IO ()
stopPlayers db pnames = mapM_ (stopPlayer db) pnames
