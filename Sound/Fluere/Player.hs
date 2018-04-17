module Sound.Fluere.Player ( newPlayer
                           , newPlayerMMap
                           , addPlayer
                           , modifyPlayerStatus
                           , play
                           , startPlayer
                           , startPlayers
                           , stopPlayer
                           , stopPlayers
                           , startAll
                           , stopAll
                           , solo
                           , startExcept
                           ) where

import Control.Monad (when)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.DataBase (getPlayerNames)
import Sound.Fluere.Action (act)
import Sound.Fluere.Pattern (nextPlayerNote)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newPlayer :: String -> String -> String -> PlayerStatus -> Player
newPlayer n pa pp ps =
    Player { playerName = n
           , playerAction = pa
           , playerPattern = pp
           , playerStatus = ps
           , playFlag = 0
           }

newPlayerMMap :: Player -> IO (MutableMap String Player)
newPlayerMMap p = fromListM [(playerName p, p)]

addPlayer :: DataBase -> Player -> IO ()
addPlayer db player = insertM (playerName player) player $ playerMMap db

---------------------------------------------------------------------
-- Modify
---------------------------------------------------------------------

modifyPlayer :: DataBase -> String -> (Player -> Player) -> IO ()
modifyPlayer db n f = do
    let pmmap = playerMMap db
    Just p <- lookupM n pmmap
    let newp = f p
    insertM n newp pmmap

modifyPlayerStatus :: DataBase -> String -> PlayerStatus -> IO ()
modifyPlayerStatus db n newps = modifyPlayer db n modifyps
    where modifyps p = p { playerStatus = newps }

modifyPlayFlag :: DataBase -> String -> Double -> IO ()
modifyPlayFlag db n newpf = modifyPlayer db n modifypf
    where modifypf p = p { playFlag = newpf }

---------------------------------------------------------------------
-- Used to perform different actions 
---------------------------------------------------------------------

play :: DataBase -> String -> Double -> IO ()
play db n t = do
    Just p <- lookupM n $ playerMMap db
    Just a <- lookupM (playerAction p) $ actionMMap db
    nextNote <- nextPlayerNote db n
    if playerStatus p == Playing
        then if nextNote == 1
                then modifyPlayFlag db n 1 >> act db p a t
                else modifyPlayFlag db n 0
        else return ()

---------------------------------------------------------------------
-- Used to perform
---------------------------------------------------------------------

startPlayer :: DataBase -> String -> IO ()
startPlayer db n = do
    Just p <- lookupM n (playerMMap db)
    if playerStatus p == Playing
        then putStrLn $ "Player " ++ n ++ " is already playing."
        else do
            modifyPlayerStatus db n Playing
            putStrLn $ "Player " ++ n ++ " starts playing."

stopPlayer :: DataBase -> String -> IO ()
stopPlayer db n = do
    Just p <- lookupM n (playerMMap db)
    if playerStatus p  == Stopping
        then putStrLn $ "Player " ++ n ++ " has been stopped."
        else do
            modifyPlayerStatus db n Stopping
            putStrLn $ "Player " ++ n ++ " stopped."

startPlayers :: DataBase -> [String] -> IO ()
startPlayers db ns = mapM_ (startPlayer db) ns

stopPlayers :: DataBase -> [String] -> IO ()
stopPlayers db ns = mapM_ (stopPlayer db) ns

startAll :: DataBase -> IO ()
startAll db = do
    pnames <- getPlayerNames db
    startPlayers db pnames

stopAll :: DataBase -> IO ()
stopAll db = do
    pnames <- getPlayerNames db
    stopPlayers db pnames

solo :: DataBase -> String -> IO ()
solo db n = do
    pnames <- getPlayerNames db
    let ps = filter (\p -> p /= n) pnames
    stopPlayers db ps
    startPlayer db n

startExcept :: DataBase -> String -> IO ()
startExcept db n = do
    pnames <- getPlayerNames db
    let ps = filter (\p -> p /= n) pnames
    startPlayers db ps
    stopPlayer db n
