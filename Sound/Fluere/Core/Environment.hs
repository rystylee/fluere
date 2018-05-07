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
               -> MutableMap String SynthDef
               -> Environment
newEnvironment n tcmmap pmmap ammap ioimmap sdmmap =
    Environment { environmentName = n
                , tempoClockMMap = tcmmap
                , playerMMap = pmmap
                , actionMMap = ammap
                , ioiSetMMap = ioimmap
                , synthDefMMap = sdmmap
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

getSynthDefNames :: Environment -> IO [String]
getSynthDefNames e = keysM $ synthDefMMap e
