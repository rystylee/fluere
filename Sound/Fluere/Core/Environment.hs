module Sound.Fluere.Core.Environment where

import Sound.Fluere.Core.MutableMap (MutableMap, keysM, elemsM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newEnvironment :: String
               -> MutableMap String TempoClock
               -> MutableMap String Player
               -> MutableMap String Action
               -> MutableMap String IOISet
               -> MutableMap String Instrument
               -> Environment
newEnvironment n tcmmap pmmap ammap ioimmap immap =
    Environment { environmentName = n
                , tempoClockMMap = tcmmap
                , playerMMap = pmmap
                , actionMMap = ammap
                , ioiSetMMap = ioimmap
                , instrumentMMap = immap
                }

---------------------------------------------------------------------
-- Used to get the keys
---------------------------------------------------------------------

getTempoClockNames :: Environment -> IO [String]
getTempoClockNames e = keysM $ tempoClockMMap e

getPlayerNames :: Environment -> IO [String]
getPlayerNames e = keysM $ playerMMap e

getPlayers :: Environment -> IO [Player]
getPlayers e = elemsM $ playerMMap e

getActionNames :: Environment -> IO [String]
getActionNames e = keysM $ actionMMap e

getIOISetNames :: Environment -> IO [String]
getIOISetNames e = keysM $ ioiSetMMap e

getInstrumentNames :: Environment -> IO [String]
getInstrumentNames e = keysM $ instrumentMMap e
