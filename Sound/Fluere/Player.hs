module Sound.Fluere.Player where

import Control.Monad (when)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Action (act)
import Sound.Fluere.Pattern (nextPlayerNote)

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newPlayer :: String -> String -> String -> Player
newPlayer n pa pp =
    Player { playerName = n
           , playerAction = pa
           , playerPattern = pp
           }

newPlayerMMap :: Player -> IO (MutableMap String Player)
newPlayerMMap p = fromListM [(playerName p, p)]

addPlayer :: DataBase -> Player -> IO ()
addPlayer db player = insertM (playerName player) player $ playerMMap db

---------------------------------------------------------------------

---------------------------------------------------------------------

play :: DataBase -> String -> Double -> IO ()
play db n t = do
    Just p <- lookupM n $ playerMMap db
    Just a <- lookupM (playerAction p) $ actionMMap db
    nextNote <- nextPlayerNote db n
    when (nextNote == 1) $ act db p a t
