module Sound.Fluere.DataBase where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data


newDataBase :: String
               -> TVar (Map String Clock)
               -> TVar (Map String Agent)
               -> TVar (Map String Action)
               -> DataBase
newDataBase dbname cmmap ammap actmmap =
    DataBase { dataBaseName = dbname
              ,clockMMap = cmmap
              ,agentMMap = ammap
              ,actionMMap = actmmap
             }
