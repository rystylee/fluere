module Sound.Fluere.DataBase where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data


newDataBase :: String
               -> TVar (Map String Clock)
               -> TVar (Map String Agent)
               -> DataBase
newDataBase dbname cmmap ammap =
    DataBase { dataBaseName = dbname
              ,clockMMap = cmmap
              ,agentMMap = ammap
             }