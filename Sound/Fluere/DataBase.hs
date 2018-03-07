module Sound.Fluere.DataBase where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data


newDataBase :: String
               -> TVar (Map String Clock)
               -> TVar (Map String Player)
               -> DataBase
newDataBase dbname cmmap pmmap =
    DataBase { dataBaseName = dbname
              ,clockMMap = cmmap
              ,playerMMap = pmmap
             }
