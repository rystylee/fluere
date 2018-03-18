module Sound.Fluere.DataBase where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data


newDataBase :: String
               -> TVar (Map String Clock)
               -> TVar (Map String Agent)
               -> TVar (Map String Action)
               -> TVar (Map String Pattern)
               -> TVar (Map String Conductor)
               -> DataBase
newDataBase dbname clmmap ammap actmmap pmmap commap =
  DataBase { dataBaseName = dbname
            ,clockMMap = clmmap
            ,agentMMap = ammap
            ,actionMMap = actmmap
            ,patternMMap = pmmap
            ,conductorMMap = commap
           }