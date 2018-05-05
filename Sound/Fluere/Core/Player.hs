module Sound.Fluere.Core.Player where

import Control.Monad (when)
import System.Random (getStdRandom, randomR)

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData
import Sound.Fluere.Core.Environment (getPlayerNames)
import Sound.Fluere.Core.IOISet (nextProb, swapIOICounter)
import Sound.Fluere.Core.Instrument (convertToOscScLang)
import Sound.Fluere.Core.Osc (sendToSC, convertToOscOFLang, sendToOF)


---------------------------------------------------------------------
-- Debug
---------------------------------------------------------------------

showAlllPlayersIOISet :: Environment -> IO ()
showAlllPlayersIOISet e = do
    pnames <- getPlayerNames e
    sequence_ $ map (\n -> showPlayerIOISet e n) pnames

showPlayerIOISet :: Environment -> String -> IO ()
showPlayerIOISet e n = do
    Just p <- lookupM n $ playerMMap e
    Just ioi <- lookupM (playerIOISet p) $ ioiSetMMap e
    putStrLn $ show ioi

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newPlayer :: String -> String -> String -> Status -> Player
newPlayer n pa pioi ps = Player { playerName = n
                                , playerAction = pa
                                , playerIOISet = pioi
                                , playerStatus = ps
                                }

newPlayerMMap :: Player -> IO (MutableMap String Player)
newPlayerMMap p = fromListM [(playerName p, p)]

addPlayer :: Environment -> Player -> IO ()
addPlayer e p = insertM (playerName p) p $ playerMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapPlayer :: Environment -> String -> (Player -> Player) -> IO ()
swapPlayer e n f = do
    let pmmap = playerMMap e
    Just p <- lookupM n pmmap
    let newp = f p
    insertM n newp pmmap

swapPlayerIOISet :: Environment -> String -> String -> IO ()
swapPlayerIOISet e n newioi = swapPlayer e n swapioi
    where swapioi p = p { playerIOISet = newioi }

swapPlayerStatus :: Environment -> String -> Status -> IO ()
swapPlayerStatus e n newps = swapPlayer e n swapps
    where swapps p = p { playerStatus = newps }

---------------------------------------------------------------------
-- Used to perform different actions 
---------------------------------------------------------------------

play :: Environment -> String -> Double -> IO ()
play e n lt = do
    Just p <- lookupM n $ playerMMap e
    Just a <- lookupM (playerAction p) $ actionMMap e
    if playerStatus p == Playing
        then act e p a lt
        else return ()

-- action
act :: Environment -> Player -> Action -> Double -> IO ()
act e p (PlaySound _ hi) lt = do
    np <- nextProb e p
    rand <- (getStdRandom $ randomR (0,1) :: IO Double)
    if rand <= np
        then do
            Just i <- lookupM hi $ instrumentMMap e
            slang <- convertToOscScLang $ instrumentParameter i
            sendToSC lt slang
            Just ioi <- lookupM (playerIOISet p) $ ioiSetMMap e
            oflang <- convertToOscOFLang ioi
            sendToOF lt oflang
        else return ()

---------------------------------------------------------------------
-- Used to perform
---------------------------------------------------------------------

startPlayer :: Environment -> String -> IO ()
startPlayer e n = do
    Just p <- lookupM n (playerMMap e)
    if playerStatus p == Playing
        then putStrLn $ "Player " ++ n ++ " is already playing."
        else do
            swapPlayerStatus e n Playing
            putStrLn $ "Player " ++ n ++ " starts playing."

stopPlayer :: Environment -> String -> IO ()
stopPlayer e n = do
    Just p <- lookupM n (playerMMap e)
    if playerStatus p  == Stopping
        then putStrLn $ "Player " ++ n ++ " has been stopped."
        else do
            swapPlayerStatus e n Stopping
            putStrLn $ "Player " ++ n ++ " stopped."

---------------------------------------------------------------------
-- batch manipulation

startPlayers :: Environment -> [String] -> IO ()
startPlayers e ns = mapM_ (startPlayer e) ns

stopPlayers :: Environment -> [String] -> IO ()
stopPlayers e ns = mapM_ (stopPlayer e) ns

startAll :: Environment -> IO ()
startAll e = do
    pnames <- getPlayerNames e
    startPlayers e pnames

stopAll :: Environment -> IO ()
stopAll e = do
    pnames <- getPlayerNames e
    stopPlayers e pnames

solo :: Environment -> String -> IO ()
solo e n = do
    pnames <- getPlayerNames e
    let ps = filter (\p -> p /= n) pnames
    stopPlayers e ps
    startPlayer e n

startExcept :: Environment -> String -> IO ()
startExcept e n = do
    pnames <- getPlayerNames e
    let ps = filter (\p -> p /= n) pnames
    startPlayers e ps
    stopPlayer e n
