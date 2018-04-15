module Sound.Fluere.Action ( newAction
                           , newActionMMap
                           , addAction
                           , act
                           ) where

import Sound.OSC.FD (Datum)
import System.Random (getStdRandom, randomR)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Osc (sendToSC)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newAction :: Action -> Action
newAction (PlaySound n om) = PlaySound { actionName = n, oscMessage = om }
newAction (ConductPlayers n ps) = ConductPlayers { actionName = n, targetPlayers = ps }

newActionMMap :: Action -> IO (MutableMap String Action)
newActionMMap (PlaySound n om) = fromListM [(n, (PlaySound n om))]
newActionMMap (ConductPlayers n ps) = fromListM [(n, (ConductPlayers n ps))]

addAction :: DataBase -> Action -> IO ()
addAction db (PlaySound n om) = insertM n (PlaySound n om) $ actionMMap db
addAction db (ConductPlayers n ps) = insertM n (ConductPlayers n ps) $ actionMMap db

---------------------------------------------------------------------
-- Different action function for each player
---------------------------------------------------------------------

act :: DataBase -> Player -> Action -> Double -> IO ()
act db p (PlaySound _ om) lo = sendToSC lo om
act db p (ConductPlayers _ tps) lo = conductPlayers db tps

---------------------------------------------------------------------
-- Action function
---------------------------------------------------------------------

conductPlayers :: DataBase -> [String] -> IO ()
conductPlayers db tps = mapM_ (conductPlayer db) tps

conductPlayer :: DataBase -> String -> IO ()
conductPlayer db n = do
    Just p <- lookupM n $ playerMMap db
    rand <- (getStdRandom $ randomR (0, 1) :: IO Double)
    if rand > 0.5
        then do
            modifyPlayerStatus' db n Stopping
            --putStrLn $ "Player " ++ n ++ " is already playing."
        else do
            modifyPlayerStatus' db n Playing
            --putStrLn $ "Player " ++ n ++ " starts playing."

---------------------------------------------------------------------
-- ToDo
-- Solve circulation import, or find some other solution.
---------------------------------------------------------------------

modifyPlayer' :: DataBase -> String -> (Player -> Player) -> IO ()
modifyPlayer' db n f = do
    let pmmap = playerMMap db
    Just p <- lookupM n pmmap
    let newp = f p
    insertM n newp pmmap

modifyPlayerStatus' :: DataBase -> String -> PlayerStatus -> IO ()
modifyPlayerStatus' db n newps = modifyPlayer' db n modifyps
    where modifyps p = p { playerStatus = newps }
