module Sound.Fluere.Player ( newPlayer
                           , newPlayerMMap
                           , addPlayer
                           , modifyPlayerStatus
                           , play
                           ) where

import Control.Monad (when)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM, lookupM)
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

---------------------------------------------------------------------

---------------------------------------------------------------------

play :: DataBase -> String -> Double -> IO ()
play db n t = do
    Just p <- lookupM n $ playerMMap db
    Just a <- lookupM (playerAction p) $ actionMMap db
    nextNote <- nextPlayerNote db n
    if playerStatus p == Playing
        then when (nextNote == 1) $ act db p a t
        else return ()
