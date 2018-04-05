module Sound.Fluere.DataBase where

import Sound.Fluere.Data
import Sound.Fluere.MutableMap (MutableMap, keysM, elemsM)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newDataBase :: MutableMap String TempoClock
            -> MutableMap String Player
            -> MutableMap String Action
            -> MutableMap String Pattern
            -> DataBase
newDataBase tcmmap plmmap ammap pammap =
    DataBase { tempoClockMMap = tcmmap
             , playerMMap = plmmap
             , actionMMap = ammap
             , patternMMap = pammap
             }

---------------------------------------------------------------------
-- Used to get the keys
---------------------------------------------------------------------

getTempoClockNames :: DataBase -> IO [String]
getTempoClockNames db = keysM $ tempoClockMMap db

getPlayerNames :: DataBase -> IO [String]
getPlayerNames db = keysM $ playerMMap db

getPlayers :: DataBase -> IO [Player]
getPlayers db = elemsM $ playerMMap db

getActionNames :: DataBase -> IO [String]
getActionNames db = keysM $ actionMMap db

getPatternNames :: DataBase -> IO [String]
getPatternNames db = keysM $ patternMMap db
